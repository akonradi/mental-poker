use std::collections::{HashMap, HashSet};

use mental_poker::game::log::LoggableGameType;
use mental_poker::game::GameType;
use mental_poker::game::InputMessageToken;
use mental_poker::game::TurnOrderProvider;
use mental_poker::OtherPlayerId;
use mental_poker::{DeckPosition, PlayerId};
// TODO remove these.
use mental_poker::util::DrawPile;
use DealState::*;
use GameState::*;
use TurnState::*;

use crate::player::GoFishStateCard;
use crate::GoFishDeck;
use crate::GoFishParseError;
use crate::{cards::*, message::*};
use log::warn;

#[derive(Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Debug)]
pub(crate) enum GoFish {}

impl GameType for GoFish {
    type Deck = GoFishDeck;
    type Action =
        crate::message::GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>;
    type Message = crate::message::GoFishAction<TransferAction, PlayerId>;
    type MessageParseError = GoFishParseError<InputMessageToken<GoFishDeck>>;
}

impl LoggableGameType for GoFish {
    type GameEvent = GameAction<(PlayerId, Vec<AttestedGoFishCard>), PlayerId>;
}

#[derive(Debug)]
pub(crate) enum GameState {
    Dealing(PlayerId, DealState),
    Running(PlayerId, TurnState),
}

#[derive(Debug)]
pub(crate) enum DealState {
    Starting,
    Drawing,
}

#[derive(Debug)]
pub(crate) enum TurnState {
    /// Starting a turn
    Playing,
    /// Waiting for cards from other player
    Awaiting(PlayerId, Rank),
    /// After receiving cards from another player
    ReceivedResponse(PlayerId, Rank, usize),
    /// Currently drawing (waiting for reveal from players)
    Fishing(Option<Rank>),
    /// After fishing, waiting for the player to possibly reveal a pair.
    AfterFishing(Rank),
}

pub(crate) struct DoneState {
    pub turn_order: Vec<PlayerId>,
    pub revealed: HashMap<PlayerId, Vec<AttestedGoFishCard>>,
}

#[derive(Debug)]
pub(crate) struct PlayerState<Card> {
    hand: Hand<Card>,
    revealed: Vec<AttestedGoFishCard>,
}

#[derive(Debug)]
pub(crate) struct GoFishState<Card> {
    players: HashMap<PlayerId, PlayerState<Card>>,
    first_player: PlayerId,
    turn_order: HashMap<PlayerId, PlayerId>,
    state: GameState,
    draw: DrawPile<GoFishDeck>,
}

impl<Card> Default for PlayerState<Card> {
    fn default() -> Self {
        Self {
            hand: Default::default(),
            revealed: Default::default(),
        }
    }
}

#[derive(derive_more::Error, Debug, derive_more::From, derive_more::Display)]
pub enum ActionError<T> {
    OutOfTurn,
    #[from]
    Action(T),
}

#[derive(Debug, derive_more::Display, derive_more::Error)]
pub enum DrawError {
    AfterReceivingSomeCards,
    WhilePlayingWithCards,
    WhileDealDrawing,
    AlreadyFishing,
    AfterFishing,
    Awaiting,
    NoMoreCards,
    DoneDealing,
}

#[derive(Debug, derive_more::Display, derive_more::Error)]
pub(crate) enum RequestError {
    Dealing,
    NoCardOfRank(Rank),
    Awaiting,
    AfterReceive,
    WhileFishing,
    AfterFishing,
}

#[derive(Debug, derive_more::Display, derive_more::Error)]
pub(crate) enum RevealError {
    CardsNotPresent,
    CardsNotPaired,
    CardsWrongRank,
    MoreUnmatched,
    MustFish,
    Fishing,
    Awaiting,
    Dealing,
    DealingNotDone,
    NoCards,
}

#[derive(Debug, derive_more::Display, derive_more::Error)]
pub(crate) enum TransferError {
    Dealing,
    WrongRecipient,
    ReceivedResponse,
    Playing,
    Fishing,
    AfterFishing,
    SendingPlayerMissingCards,
    DifferentRanks,
    WrongRank,
    SomeCardsUntransferred,
}

#[derive(Debug, derive_more::Display, derive_more::Error)]
pub(crate) enum DrewError {
    WrongCard,
    NotDrawing,
}

#[derive(Debug, derive_more::IntoIterator)]
pub(crate) struct Hand<Card>(Vec<Card>);

impl<Card> Default for Hand<Card> {
    fn default() -> Self {
        Self(Default::default())
    }
}

impl<Card> Hand<Card> {
    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn extend(&mut self, removed: Vec<Card>) {
        self.0.extend(removed)
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = &Card> {
        self.0.iter()
    }
}

impl<Card> AsRef<[Card]> for Hand<Card> {
    fn as_ref(&self) -> &[Card] {
        &self.0
    }
}

impl<Card: GoFishStateCard> Hand<Card> {
    pub(crate) fn matching_rank(&self, rank: Rank) -> impl Iterator<Item = &Card> {
        self.0.iter().filter(move |c| c.card_rank() == rank.into())
    }

    pub(crate) fn take(&mut self, cards: &[Card]) -> Option<Vec<Card>> {
        let mut taken = Vec::new();
        for c in cards {
            let p = c.card_value();
            let found = self
                .0
                .iter()
                .enumerate()
                .find_map(|(i, c)| (c.card_value() == p).then_some(i));
            match found {
                Some(i) => {
                    taken.push(self.0.swap_remove(i));
                }
                None => {
                    self.0.extend(taken);
                    return None;
                }
            }
        }
        Some(taken)
    }

    pub(crate) fn remove_cards(&mut self, cards: &[AttestedGoFishCard]) -> Option<Vec<Card>> {
        match cards.into_iter().try_fold(Vec::new(), |mut removed, a| {
            match self
                .0
                .iter()
                .enumerate()
                .find_map(|(i, c)| (c.card_value() == a.card_value().into()).then_some(i))
            {
                Some(i) => {
                    removed.push(self.0.swap_remove(i));
                    Ok(removed)
                }
                None => Err(removed),
            }
        }) {
            Ok(removed) => Some(removed),
            Err(removed) => {
                self.0.extend(removed);
                None
            }
        }
    }

    pub(crate) fn pairs(&self) -> impl Iterator<Item = [&Card; 2]> {
        let mut ranks = HashMap::new();
        let mut it = self.0.iter();
        std::iter::from_fn(move || {
            while let Some(c) = it.next() {
                match ranks.entry(c.card_rank()) {
                    std::collections::hash_map::Entry::Occupied(o) => return Some([c, o.remove()]),
                    std::collections::hash_map::Entry::Vacant(v) => {
                        let _ = v.insert(c);
                    }
                }
            }
            None
        })
    }

    pub(crate) fn take_pairs(&mut self) -> Vec<[Card; 2]> {
        let mut ranks = HashMap::new();
        let mut it = std::mem::take(&mut self.0).into_iter();
        let mut out = Vec::new();

        while let Some(c) = it.next() {
            match ranks.entry(c.card_rank()) {
                std::collections::hash_map::Entry::Occupied(o) => out.push([c, o.remove()]),
                std::collections::hash_map::Entry::Vacant(v) => {
                    let _ = v.insert(c);
                }
            }
        }

        self.0 = ranks.into_values().collect();
        out
    }
}

impl Hand<AttestedGoFishCard> {
    pub(crate) fn take_matching(&mut self, rank: Rank) -> Vec<AttestedGoFishCard> {
        let (taken, kept) = std::mem::take(&mut self.0)
            .into_iter()
            .partition(|c| GoFishDeckCard::rank(c) == rank);
        self.0 = kept;
        taken
    }
}

impl<Card> PlayerState<Card> {
    pub(crate) fn hand(&self) -> &Hand<Card> {
        &self.hand
    }

    pub fn revealed(&self) -> &[AttestedGoFishCard] {
        &self.revealed
    }
}

impl<Card: GoFishStateCard> GoFishState<Card> {
    pub fn new(turn_order: Vec<PlayerId>) -> Self {
        let players = turn_order
            .iter()
            .map(|i| (*i, PlayerState::default()))
            .collect();
        let first_player = *turn_order.first().unwrap();
        let turn_order = (0..turn_order.len())
            .map(|i| (turn_order[i], turn_order[(i + 1) % turn_order.len()]))
            .collect();

        Self {
            state: Dealing(first_player, Starting),
            first_player,
            turn_order,
            players,
            draw: DrawPile::new(),
        }
    }

    pub fn player_hand(&self, player_id: &PlayerId) -> &Hand<Card> {
        &self.players[player_id].hand
    }

    pub fn deal_size(&self) -> usize {
        (self.players.len() == 2).then_some(7).unwrap_or(5)
    }

    pub fn state(&self) -> &GameState {
        &self.state
    }

    pub fn game_over(&self) -> bool {
        self.players.values().all(|p| p.hand.len() == 0) && self.draw.top() == None
    }

    pub fn top_of_deck(&self) -> Option<&DeckPosition<GoFishDeck>> {
        self.draw.top()
    }

    pub fn players(&self) -> impl Iterator<Item = (&PlayerId, &PlayerState<Card>)> {
        self.players.iter()
    }

    fn next_player(&self, player: PlayerId) -> PlayerId {
        self.turn_order[&player]
    }

    /// Called when a player is seen to start drawing a card.
    pub fn draw(&mut self, who: PlayerId) -> Result<(), ActionError<DrawError>> {
        match self.state {
            Dealing(p, _) | Running(p, _) if p != who => Err(ActionError::OutOfTurn),

            Dealing(p, Starting) => {
                let hand = &self.players.get(&p).unwrap().hand;
                if hand.len() == self.deal_size() {
                    return Err(DrawError::DoneDealing.into());
                }
                let hand = &self.players.get_mut(&p).unwrap().hand;
                assert_ne!(hand.len(), self.deal_size());
                if let None = self.draw.top() {
                    warn!("can't deal from empty deck");
                    return Err(DrawError::NoMoreCards.into());
                };
                self.state = Dealing(p, Drawing);
                Ok(())
            }

            Running(p, ReceivedResponse(_, rank, num_cards)) => {
                if p == who {
                    if num_cards == 0 {
                        if self.draw.top() == None {
                            return Err(DrawError::NoMoreCards.into());
                        }
                        self.state = Running(p, Fishing(Some(rank)));
                        Ok(())
                    } else {
                        Err(DrawError::AfterReceivingSomeCards.into())
                    }
                } else {
                    Err(ActionError::OutOfTurn)
                }
            }
            Running(p, Playing) => {
                if self.players[&p].hand.len() == 0 {
                    if self.draw.top() == None {
                        return Err(DrawError::NoMoreCards.into());
                    }
                    self.state = Running(p, Fishing(None));
                    Ok(())
                } else {
                    Err(DrawError::WhilePlayingWithCards.into())
                }
            }
            Dealing(_, Drawing) => Err(DrawError::WhileDealDrawing.into()),
            Running(_, Fishing(_)) => Err(DrawError::AlreadyFishing.into()),
            Running(_, Awaiting(_, _)) => Err(DrawError::Awaiting.into()),
            Running(_, AfterFishing(_)) => Err(DrawError::AfterFishing.into()),
        }
    }

    pub fn request(
        &mut self,
        from: PlayerId,
        to: PlayerId,
        rank: Rank,
    ) -> Result<(), ActionError<RequestError>> {
        match self.state {
            Dealing(p, _) | Running(p, _) if p != from => Err(ActionError::OutOfTurn),
            Dealing(_, _) => Err(RequestError::Dealing.into()),

            Running(p, Playing) => match self.players[&p].hand.matching_rank(rank).next() {
                Some(_) => {
                    self.state = Running(p, Awaiting(to, rank));
                    Ok(())
                }
                None => Err(RequestError::NoCardOfRank(rank).into()),
            },

            Running(_, Awaiting(_, _)) => Err(RequestError::Awaiting.into()),
            Running(_, ReceivedResponse(_, _, _)) => Err(RequestError::AfterReceive.into()),
            Running(_, Fishing(_)) => Err(RequestError::WhileFishing.into()),
            Running(_, AfterFishing(_)) => Err(RequestError::AfterFishing.into()),
        }
    }

    fn remove_matching_cards(
        hand: &mut Hand<Card>,
        cards: &[AttestedGoFishCard],
    ) -> Result<Vec<Card>, RevealError> {
        match hand.remove_cards(cards) {
            None => Err(RevealError::CardsNotPresent),
            Some(removed) => {
                let removed_rank_counts =
                    removed.iter().fold(HashMap::new(), |mut counts, card| {
                        *counts.entry(card.card_rank()).or_insert(0u32) += 1;
                        counts
                    });
                if removed_rank_counts.into_iter().all(|(_, n)| n % 2 == 0) {
                    Ok(removed)
                } else {
                    hand.extend(removed);
                    Err(RevealError::CardsNotPaired)
                }
            }
        }
    }

    pub fn transfer(
        &mut self,
        from: PlayerId,
        to: PlayerId,
        cards: &[Card],
    ) -> Result<(), ActionError<TransferError>> {
        match self.state {
            Dealing(_, _) => Err(TransferError::Dealing.into()),
            Running(p, _) if p != to => Err(TransferError::WrongRecipient.into()),
            Running(_, Awaiting(p, _)) if p != from => Err(ActionError::OutOfTurn),

            Running(to, Awaiting(from, rank)) => {
                if let Some(first) = cards.first() {
                    if !cards.iter().all(|c| c.card_rank() == first.card_rank()) {
                        return Err(TransferError::DifferentRanks.into());
                    }
                }
                if cards.iter().any(|c| c.card_rank() != rank.into()) {
                    return Err(TransferError::WrongRank.into());
                }
                let from_hand = &mut self.players.get_mut(&from).unwrap().hand;
                let cards = from_hand
                    .take(cards)
                    .ok_or::<ActionError<_>>(TransferError::SendingPlayerMissingCards.into())?;
                if Card::DISTINGUISHABLE && from_hand.matching_rank(rank).count() > 0 {
                    return Err(TransferError::SomeCardsUntransferred.into());
                }
                self.state = Running(to, TurnState::ReceivedResponse(from, rank, cards.len()));
                let to = self.players.get_mut(&to).unwrap();
                to.hand.extend(cards);
                Ok(())
            }

            Running(_, ReceivedResponse(_, _, _)) => Err(TransferError::ReceivedResponse.into()),
            Running(_, Playing) => Err(TransferError::Playing.into()),
            Running(_, Fishing(_)) => Err(TransferError::Fishing.into()),
            Running(_, AfterFishing(_)) => Err(TransferError::AfterFishing.into()),
        }
    }

    pub fn reveal(
        &mut self,
        from: PlayerId,
        cards: &[AttestedGoFishCard],
    ) -> Result<(), ActionError<RevealError>> {
        let deal_size = self.deal_size();
        match self.state {
            Dealing(p, _) | Running(p, _) if p != from => Err(ActionError::OutOfTurn),
            Dealing(_, Drawing) => Err(RevealError::Dealing.into()),

            Dealing(_, Starting) => {
                let player = self.players.get_mut(&from).unwrap();
                let hand = &mut player.hand;
                if hand.len() != deal_size {
                    return Err(RevealError::DealingNotDone.into());
                }
                let removed = Self::remove_matching_cards(hand, cards)?;
                if Card::DISTINGUISHABLE {
                    // Check for extra pairs that weren't revealed.
                    if hand.pairs().count() != 0 {
                        hand.extend(removed);
                        return Err(RevealError::MoreUnmatched.into());
                    }
                }

                player.revealed.extend(cards.into_iter());
                let next_player = self.next_player(from);
                if next_player == self.first_player() {
                    self.state = Running(next_player, Playing);
                } else {
                    self.state = Dealing(next_player, Starting);
                }
                Ok(())
            }
            Running(_, Awaiting(_, _)) => Err(RevealError::Awaiting.into()),
            Running(p, ReceivedResponse(_, rank, _)) => {
                if !cards.iter().all(|c| GoFishDeckCard::rank(c) == rank) {
                    Err(RevealError::CardsWrongRank.into())
                } else if cards.len() == 0 && self.draw.top().is_some() {
                    Err(RevealError::MustFish.into())
                } else {
                    let player = self.players.get_mut(&from).unwrap();
                    let hand = &mut player.hand;
                    let removed = Self::remove_matching_cards(hand, cards)?;
                    if removed.len() == 0 {
                        self.state = Running(self.next_player(p), Playing);
                    } else {
                        player.revealed.extend(cards.into_iter());
                        self.state = Running(p, Playing);
                    }
                    Ok(())
                }
            }
            Running(p, Playing) => {
                match cards
                    .iter()
                    .map(|c| GoFishDeckCard::rank(c))
                    .collect::<HashSet<_>>()
                    .len()
                {
                    0 => {
                        if self.draw.top().is_some() {
                            return Err(RevealError::NoCards.into());
                        }
                    }
                    1 => (),
                    _more_than_1 => return Err(RevealError::CardsWrongRank.into()),
                }

                let player = self.players.get_mut(&from).unwrap();
                let hand = &mut player.hand;
                let removed = Self::remove_matching_cards(hand, cards)?;
                if removed.len() == 0 {
                    self.state = Running(self.next_player(p), Playing);
                    Ok(())
                } else {
                    player.revealed.extend(cards.into_iter());
                    self.state = Running(p, Playing);
                    Ok(())
                }
            }

            Running(_, Fishing(_)) => Err(RevealError::Fishing.into()),
            Running(p, AfterFishing(rank)) => {
                if !cards.iter().all(|c| GoFishDeckCard::rank(c) == rank) {
                    return Err(RevealError::CardsWrongRank.into());
                }
                if cards.is_empty() {
                    self.state = Running(self.next_player(p), Playing);
                    Ok(())
                } else {
                    let player = self.players.get_mut(&from).unwrap();
                    let hand = &mut player.hand;
                    let removed = Self::remove_matching_cards(hand, cards)?;
                    if removed.len() == 0 {
                        self.state = Running(self.next_player(p), Playing);
                        Ok(())
                    } else {
                        player.revealed.extend(cards.into_iter());
                        self.state = Running(p, Playing);
                        Ok(())
                    }
                }
            }
        }
    }

    pub(crate) fn drew(
        &mut self,
        who: PlayerId,
        pos: DeckPosition<GoFishDeck>,
        card: Card,
    ) -> Result<(), ActionError<DrewError>> {
        match self.state {
            Dealing(p, _) | Running(p, _) if p != who => Err(ActionError::OutOfTurn),
            Dealing(_, Drawing) => {
                if self.draw.top() != Some(&pos) {
                    Err(DrewError::WrongCard.into())
                } else {
                    let player = &mut self.players.get_mut(&who).unwrap();
                    player.hand.extend(vec![card]);
                    self.draw.next();
                    self.state = Dealing(who, Starting);
                    Ok(())
                }
            }
            Running(_, Fishing(rank)) => {
                if self.draw.top() != Some(&pos) {
                    Err(DrewError::WrongCard.into())
                } else {
                    let player = &mut self.players.get_mut(&who).unwrap();
                    player.hand.extend(vec![card]);
                    self.draw.next();
                    self.state = match rank {
                        Some(rank) => Running(who, AfterFishing(rank)),
                        None => Running(self.next_player(who), Playing),
                    };
                    Ok(())
                }
            }
            Dealing(_, _) => Err(DrewError::NotDrawing.into()),
            Running(_, _) => todo!(),
        }
    }

    pub(crate) fn end_game(self) -> Result<DoneState, Self> {
        if self.game_over() {
            let turn_order = self.turn_order().collect();
            let revealed = self
                .players
                .into_iter()
                .map(|(id, state)| (id, state.revealed))
                .collect();
            Ok(DoneState {
                revealed,
                turn_order,
            })
        } else {
            Err(self)
        }
    }

    fn first_player(&self) -> PlayerId {
        self.first_player
    }
}

impl<'s, G> TurnOrderProvider for &'s GoFishState<G> {
    type Iter = impl Iterator<Item = PlayerId>;
    fn turn_order(self) -> Self::Iter {
        let mut next = self.first_player;
        std::iter::from_fn(move || {
            let r = next;
            next = self.turn_order[&next];
            Some(r)
        })
        .take(self.players.len())
    }
}

impl std::fmt::Display for GameState {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fn who(p: &PlayerId, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match p {
                PlayerId::This => f.write_str("you"),
                PlayerId::Other(o) => f.write_fmt(format_args!("player {}", u8::from(o))),
            }
        }
        fn whose(p: &PlayerId, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match p {
                PlayerId::This => f.write_str("your"),
                PlayerId::Other(o) => f.write_fmt(format_args!("player {}'s", u8::from(o))),
            }
        }
        fn who_is(p: &PlayerId, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match p {
                PlayerId::This => f.write_str("you are"),
                PlayerId::Other(o) => f.write_fmt(format_args!("player {} is", u8::from(o))),
            }
        }

        match self {
            Dealing(p, Starting) => f.write_str("dealing to ").and_then(|_| who(p, f)),
            Dealing(p, Drawing) => f.write_str("dealt to ").and_then(|_| who(p, f)),
            Running(p, Playing) => whose(p, f).and_then(|_| f.write_str(" turn")),
            Running(p, Awaiting(o, r)) => who_is(p, f).and_then(|_| {
                f.write_fmt(format_args!(
                    " waiting for cards of rank {} from ",
                    u8::from(*r),
                ))
                .and_then(|_| who(o, f))
            }),
            Running(p, Fishing(_)) => who_is(p, f).and_then(|_| f.write_str(" fishing")),
            Running(p, AfterFishing(_)) => who(p, f).and_then(|_| f.write_str(" finished fishing")),
            Running(p, ReceivedResponse(o, r, n)) => who(p, f).and_then(|_| {
                f.write_fmt(format_args!(
                    " got {} cards of rank {} from ",
                    n,
                    u8::from(*r)
                ))
                .and_then(|_| who(o, f))
            }),
        }
    }
}

impl From<GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>>
    for GoFishAction<TransferAction, PlayerId>
{
    fn from(action: GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>) -> Self {
        match action {
            GoFishAction::Draw => Self::Draw,
            GoFishAction::Reveal(cards) => GoFishAction::Reveal(cards),
            GoFishAction::Request { to, rank } => GoFishAction::Request {
                to: to.into(),
                rank,
            },
            GoFishAction::Transfer((p, cards)) => {
                GoFishAction::Transfer(TransferAction::ToOther(p, cards.len()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use std::iter::Step as _;

    use assert_matches::assert_matches;
    use mental_poker::game::trusting::Bootstrap;

    use super::*;
    use crate::{bootstrap_games, player::Player};

    #[test]
    fn must_go_fish_after_failed_ask() {
        let mut bootstrap = Bootstrap::new();
        let other = bootstrap.add_player(1).unwrap().into();
        let mut state = GoFishState::<UnknownCard>::new(vec![PlayerId::This, other]);

        // Get through the initial dealing.
        let mut deck_top = DeckPosition::start();
        for _ in 0..state.deal_size() {
            state.draw(PlayerId::This).unwrap();
            state.drew(PlayerId::This, deck_top, UnknownCard).unwrap();
            deck_top = DeckPosition::forward(deck_top, 1);
        }
        state.reveal(PlayerId::This, &[]).unwrap();

        for _ in 0..state.deal_size() {
            state.draw(other).unwrap();
            state.drew(other, deck_top, UnknownCard).unwrap();
            deck_top = DeckPosition::forward(deck_top, 1);
        }
        state.reveal(other, &[]).unwrap();

        // Ask the other player for a card. When that player doesn't have it,
        // This player MUST draw.

        state
            .request(PlayerId::This, other, 3.try_into().unwrap())
            .unwrap();
        state.transfer(other, PlayerId::This, &[]).unwrap();

        // This should not be allowed; This player MUST go fish instead of
        // ending their turn.
        assert_matches!(
            state.reveal(PlayerId::This, &[]),
            Err(ActionError::Action(RevealError::MustFish))
        );

        // The player should be able to start fishing successfully.
        state.draw(PlayerId::This).unwrap();
    }
}
