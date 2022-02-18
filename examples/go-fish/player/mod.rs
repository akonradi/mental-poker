use std::{
    collections::{HashMap, HashSet},
    fmt::Write,
};

use log::info;
use mental_poker::{
    game::{
        log::{
            DeckProvider, FinishedCardGame as _, GameMessageDecoder, LoggableGameType, LoggingGame,
        },
        AttestedCard, CardGame, GameInput,
    },
    Card, DeckPosition, DeckType, OtherPlayerId, PlayerId,
};

use crate::{
    cards::{
        AttestedGoFishCard, GoFishDeck, GoFishDeckCard, Rank, SomeRank, SomeValue, UnknownCard,
    },
    game::{
        ActionError, DoneState, DrawError, DrewError, GameState, GoFish, GoFishState, Hand,
        RequestError, RevealError, TransferError, TurnState,
    },
    message::{GameAction, GoFishAction, TransferAction},
};

mod auto;
mod cli;

pub(crate) use auto::AutoPlayer;
pub(crate) use cli::CliPlayer;

pub(crate) trait GoFishStateCard {
    type Rank: Eq + std::hash::Hash + From<Rank>;
    type Value: Eq + std::hash::Hash + From<Card<GoFishDeck>>;
    const DISTINGUISHABLE: bool;
    fn card_rank(&self) -> Self::Rank;
    fn card_value(&self) -> Self::Value;
}

impl GoFishStateCard for UnknownCard {
    type Rank = SomeRank;
    const DISTINGUISHABLE: bool = false;

    fn card_rank(&self) -> Self::Rank {
        Self::Rank {}
    }

    type Value = SomeValue;

    fn card_value(&self) -> Self::Value {
        Self::Value {}
    }
}

impl GoFishStateCard for AttestedGoFishCard {
    type Rank = Rank;
    const DISTINGUISHABLE: bool = true;

    fn card_rank(&self) -> Self::Rank {
        GoFishDeckCard::rank(self)
    }

    type Value = Card<GoFishDeck>;

    fn card_value(&self) -> Self::Value {
        *Into::<AttestedCard<_>>::into(*self).card()
    }
}

#[derive(Debug)]
enum PlayerAction {
    Do(GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>),
    RevealCard(OtherPlayerId, DeckPosition<GoFishDeck>),
    EndGame,
}

impl From<GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>> for PlayerAction {
    fn from(action: GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>) -> Self {
        Self::Do(action)
    }
}

trait PlayerImpl<G: CardGame> {
    fn action(&mut self, state: &mut GoFishState<UnknownCard>) -> Option<PlayerAction>;
    fn receive(&mut self, cards: Vec<AttestedGoFishCard>);
    fn drew(&mut self, card: AttestedGoFishCard);
    fn hand(&self) -> &Hand<AttestedGoFishCard>;
}

struct PlayerHand<'a, T>(&'a Hand<T>);

impl<'a> std::fmt::Display for PlayerHand<'a, AttestedGoFishCard> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let cards = Rank::all()
            .map(|r| (r, self.0.matching_rank(r)))
            .filter_map(|(rank, cards)| {
                let mut cards = cards.peekable();
                if cards.peek().is_some() {
                    Some((rank, cards))
                } else {
                    None
                }
            });

        f.write_char('[')?;
        let mut remaining_cards = self.0.len();
        for (rank, cards) in cards {
            write!(f, "{rank}: {{")?;
            let mut it = cards.cloned().peekable();
            while let Some(card) = it.next() {
                let card: AttestedCard<_> = card.into();
                write!(f, "{}", u16::from(card.card()))?;
                if it.peek().is_some() {
                    write!(f, ", ")?;
                }
                remaining_cards -= 1;
            }
            write!(f, "}}")?;
            if remaining_cards > 0 {
                write!(f, ", ")?;
            }
        }
        f.write_char(']')
    }
}

impl<'a> std::fmt::Display for PlayerHand<'a, UnknownCard> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("{} cards", self.0.len()))
    }
}

struct OtherPlayerHands<'a>(HashMap<&'a OtherPlayerId, &'a Hand<UnknownCard>>);

impl<'a> std::fmt::Display for OtherPlayerHands<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut keys = self.0.keys().collect::<Vec<_>>();
        keys.sort_by_key::<u8, _>(|o| (**o).into());
        f.debug_list()
            .entries(
                keys.into_iter()
                    .map(|k| format!("Player {}: {} cards", u8::from(*k), self.0[k].len())),
            )
            .finish()
    }
}

enum PlayingState {
    InProgress(GoFishState<UnknownCard>),
    Done(DoneState),
    Borrowed,
}

pub(crate) struct Player<G: CardGame>
where
    G::Game: LoggableGameType,
{
    game: LoggingGame<G>,
    player: Box<dyn PlayerImpl<G>>,
    state: PlayingState,
}

pub(crate) enum PlayerType {
    Cli,
    Auto,
    #[cfg(test)]
    AutoCheating(usize),
}

impl<G: CardGame<Game = GoFish>> Player<G>
where
    G::InputError: 'static,
{
    pub(crate) fn new(i: u8, game: G, turn_order: Vec<PlayerId>, player_type: PlayerType) -> Self {
        let player: Box<dyn PlayerImpl<_>> = match player_type {
            PlayerType::Auto => Box::new(AutoPlayer::new()),
            PlayerType::Cli => Box::new(CliPlayer::new(i)),
            #[cfg(test)]
            PlayerType::AutoCheating(num_cheats) => Box::new(AutoPlayer::new_cheater(num_cheats)),
        };

        let game = LoggingGame::new(game);
        Self {
            player,
            game,
            state: PlayingState::InProgress(GoFishState::new(turn_order)),
        }
    }

    pub(crate) fn state(&self) -> Option<&GoFishState<UnknownCard>> {
        match &self.state {
            PlayingState::InProgress(p) => Some(&p),
            PlayingState::Done(_) => None,
            PlayingState::Borrowed => unreachable!(),
        }
    }

    pub(crate) fn hand(&self) -> &Hand<AttestedGoFishCard> {
        self.player.hand()
    }

    pub(crate) fn actions(&mut self) -> Result<Option<G::GameMessage>, Box<dyn std::error::Error>> {
        let Self {
            state,
            player,
            game,
        } = self;
        let state = match state {
            PlayingState::InProgress(state) => state,
            PlayingState::Done(_) => return Ok(None),
            PlayingState::Borrowed => unreachable!(),
        };
        let action = match player.action(state) {
            Some(action) => action,
            None => return Ok(None),
        };

        match action {
            PlayerAction::Do(action) => {
                Self::handle(state, player, PlayerId::This, action.clone().into())?;
                let message = game.send(action).unwrap();
                Ok(Some(message))
            }
            PlayerAction::RevealCard(to, pos) => {
                state.drew(to.into(), pos, UnknownCard {}).unwrap();
                Ok(Some(game.reveal(to, pos)))
            }
            PlayerAction::EndGame => {
                match std::mem::replace(&mut self.state, PlayingState::Borrowed) {
                    PlayingState::InProgress(state) => match state.end_game() {
                        Ok(done) => self.state = PlayingState::Done(done),
                        Err(e) => {
                            self.state = PlayingState::InProgress(e);
                            return Err("not finished".into());
                        }
                    },
                    PlayingState::Done(state) => {
                        self.state = PlayingState::Done(state);
                        return Err("already finished".into());
                    }
                    PlayingState::Borrowed => unreachable!(),
                };
                Ok(None)
            }
        }
    }

    fn handle(
        state: &mut GoFishState<UnknownCard>,
        player: &mut Box<dyn PlayerImpl<G>>,
        from: PlayerId,
        action: GoFishAction<TransferAction, PlayerId>,
    ) -> Result<(), Box<dyn std::error::Error>> {
        match action {
            GoFishAction::Draw => {
                state.draw(from.into())?;
                Ok(())
            }
            GoFishAction::Reveal(cards) => match state.reveal(from.into(), &cards) {
                Ok(_) => Ok(()),
                Err(e) => {
                    player.receive(cards);
                    return Err(e.into());
                }
            },
            GoFishAction::Request { to, rank } => Ok(state.request(from.into(), to, rank)?),
            GoFishAction::Transfer(TransferAction::ToOther(to, num_cards)) => {
                let mut moved = Vec::new();
                moved.resize(num_cards, Default::default());
                Ok(state.transfer(from.into(), to.into(), &moved)?)
            }
            GoFishAction::Transfer(TransferAction::ToSelf(cards)) => {
                if let GameState::Running(PlayerId::This, TurnState::Awaiting(_, rank)) =
                    state.state()
                {
                    if cards.iter().any(|c| c.rank() != *rank) {
                        return Err(format!("wrong rank, expected {}", rank).into());
                    }
                }
                {
                    let mut moved = Vec::new();
                    moved.resize(cards.len(), Default::default());
                    state.transfer(from.into(), PlayerId::This, &moved)?
                }
                player.receive(cards);
                Ok(())
            }
        }
    }

    pub(crate) fn receive(
        &mut self,
        message: G::GameMessage,
    ) -> Result<(), Box<dyn std::error::Error>> {
        let message = self.game.receive(message)?;
        let message = match message {
            Some(m) => m,
            None => return Ok(()),
        };
        let state = match &mut self.state {
            PlayingState::InProgress(state) => state,
            PlayingState::Done(_) => return Err("game is over".into()),
            PlayingState::Borrowed => unreachable!(),
        };
        match message {
            GameInput::Message(from, message) => {
                Self::handle(state, &mut self.player, from.into(), message)
            }
            GameInput::Reveal(card) => {
                state
                    .drew(PlayerId::This, *card.position(), UnknownCard {})
                    .unwrap();
                self.player.drew(card.into());
                Ok(())
            }
        }
    }

    pub fn playing(&self) -> bool {
        match self.state {
            PlayingState::InProgress(_) => true,
            PlayingState::Done(_) => false,
            PlayingState::Borrowed => unreachable!(),
        }
    }
}

#[derive(thiserror::Error, Debug, derive_more::Display)]
pub(crate) enum ValidationError {
    StillInProgress,
    FinishError(Box<dyn std::error::Error>),
    DecodeError(Box<dyn std::error::Error>),
    DrawError(#[from] ActionError<DrawError>),
    RevealError(#[from] ActionError<RevealError>),
    RequestError(#[from] ActionError<RequestError>),
    TransferError(#[from] ActionError<TransferError>),
    DrewError(#[from] ActionError<DrewError>),
    GameNotOver,
}

impl<G: CardGame<Game = GoFish>> Player<G>
where
    G::Game: LoggableGameType,
    G::FinishedGame: GameMessageDecoder<G> + DeckProvider<GoFishDeck>,
    <G::FinishedGame as GameMessageDecoder<G>>::DecodeError: 'static,
{
    pub(crate) fn validate(self) -> Result<(), ValidationError> {
        use ValidationError::*;
        let Self {
            game,
            state,
            player: _,
        } = self;

        let state = match state {
            PlayingState::InProgress(_) => return Err(StillInProgress),
            PlayingState::Done(done) => done,
            PlayingState::Borrowed => unreachable!(),
        };

        let DoneState {
            turn_order,
            revealed,
        } = state;

        let info = G::FinishInput::default();
        let finished_game = match game.finish(info) {
            Ok(finished) => finished,
            Err(e) => return Err(FinishError(e.into())),
        };
        let deck = finished_game.game().cards();
        let mut game = GoFishState::<AttestedGoFishCard>::new(turn_order);
        let mut draws = [(); <GoFishDeck as DeckType>::SIZE].map(|_| HashSet::new());
        for entry in finished_game.log() {
            let event = entry.map_err(|e| DecodeError(e.into()))?;
            match event {
                GameAction::Game { from, action } => {
                    info!("verifying from {:?}: {:?}", from, action);
                    match Into::into(action) {
                        GoFishAction::Draw => game.draw(from)?,
                        GoFishAction::Reveal(cards) => game.reveal(from, &cards)?,
                        GoFishAction::Request { to, rank } => game.request(from, to, rank)?,
                        GoFishAction::Transfer((to, cards)) => game.transfer(from, to, &cards)?,
                    }
                }
                GameAction::RevealDrawn { from, to, pos } => {
                    let draw = &mut draws[pos];
                    draw.extend([from, to]);
                    if draw.len() == game.players().count() {
                        info!("verifying draw for {:?} at {:?}", to, pos);
                        game.drew(to, pos, deck[pos].into())?
                    }
                }
            }
        }
        let done = match game.end_game() {
            Ok(done) => done,
            Err(_) => return Err(GameNotOver),
        };
        if done.revealed != revealed {
            todo!()
        }
        Ok(())
    }
}
