use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    convert::Infallible,
    fmt::Debug,
    marker::PhantomData,
};

use ::log::{info, trace, warn};
use derive_where::derive_where;
use rand::seq::SliceRandom;
use sha2::{digest::generic_array, Digest, Sha256};
use thiserror::Error;

use crate::{
    game::{
        log::{DeckProvider, GameMessageDecoder, LoggableGameType},
        AttestedCard, AttestedDeckCard, CardGame, GameInput, GameType, HiddenMessageToken,
        InputMessage, InputMessageToken, OutputMessage, OutputMessageToken, PrivateMessageToken,
        PublicMessageToken, RevealedMessageToken, TurnOrderProvider,
    },
    Card, DeckPosition, DeckType, OtherPlayerId, PlayerId,
};

use super::{PublicEvent, RevealedMessage};

#[derive_where(Debug, Clone)]
enum Token<G: GameType>
where
    [(); G::Deck::SIZE]:,
{
    PublicAttestedCard(AttestedCard<G::Deck>),
    PublicPlayer(<Game<G> as CardGame>::UniqueId),
    PublicValue(u64),
    PrivateAttestedCard(<Game<G> as CardGame>::UniqueId, AttestedCard<G::Deck>),
}

#[derive_where(Debug)]
pub struct PlayerMessage<G: GameType>(Vec<Token<G>>)
where
    [(); G::Deck::SIZE]:;

impl<G: GameType> Clone for PlayerMessage<G>
where
    [(); G::Deck::SIZE]:,
{
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

#[derive(Error)]
#[derive_where(Copy, Clone, Debug; ID: Copy + Debug)]
pub enum PlayerMessageError<ID, D: DeckType> {
    #[error("unknown player ID {0}")]
    UnknownPlayer(ID),
    #[error("card {0:?} has not been seen")]
    UnknownCard(Card<D>),
}

impl<G: GameType> PlayerMessage<G>
where
    [(); G::Deck::SIZE]:,
{
    pub fn new(
        message: OutputMessage<G::Deck>,
        self_id: &<Game<G> as CardGame>::UniqueId,
        players: &HashMap<OtherPlayerId, <Game<G> as CardGame>::UniqueId>,
        cards: &[bool; G::Deck::SIZE],
    ) -> Result<Self, (usize, PlayerMessageError<OtherPlayerId, G::Deck>)> {
        message
            .into_iter()
            .enumerate()
            .try_fold(Vec::new(), |mut out, (i, token)| {
                let token = Token::new(token, self_id, players, cards).map_err(|e| (i, e))?;
                out.push(token);
                Ok(out)
            })
            .map(PlayerMessage)
    }

    pub fn into(
        self,
        self_id: &<Game<G> as CardGame>::UniqueId,
        players: &HashMap<<Game<G> as CardGame>::UniqueId, OtherPlayerId>,
    ) -> Result<InputMessage<G::Deck>, (usize, PlayerMessageError<UniqueId, G::Deck>)> {
        self.0
            .into_iter()
            .enumerate()
            .try_fold(Vec::new(), |mut out, (i, token)| {
                let token = token.into(self_id, players).map_err(|e| (i, e))?;
                out.push(token);
                Ok(out)
            })
            .map(|v| v.into_iter().collect())
    }
}

impl<G: GameType> Token<G>
where
    [(); G::Deck::SIZE]:,
{
    fn new(
        token: OutputMessageToken<G::Deck>,
        self_id: &<Game<G> as CardGame>::UniqueId,
        players: &HashMap<OtherPlayerId, <Game<G> as CardGame>::UniqueId>,
        cards: &[bool; G::Deck::SIZE],
    ) -> Result<Self, PlayerMessageError<OtherPlayerId, G::Deck>> {
        use OutputMessageToken::*;
        use PlayerMessageError::*;
        use Token::*;
        match token {
            Public(token) => {
                use PublicMessageToken::*;
                match token {
                    Player(p) => Ok(PublicPlayer(match p {
                        PlayerId::This => *self_id,
                        PlayerId::Other(p) => *players.get(&p).ok_or(UnknownPlayer(p))?,
                    })),
                    Value(v) => Ok(PublicValue(v)),
                    AttestedCard(card) => {
                        if cards[card.card()] {
                            Ok(PublicAttestedCard(card))
                        } else {
                            Err(PlayerMessageError::UnknownCard(*card.card()))
                        }
                    }
                }
            }
            Private(to, token) => {
                use PrivateMessageToken::*;
                let to = *players.get(&to).ok_or(UnknownPlayer(to))?;
                match token {
                    AttestedCard(card) => {
                        if cards[card.card()] {
                            Ok(PrivateAttestedCard(to, card))
                        } else {
                            Err(PlayerMessageError::UnknownCard(*card.card()))
                        }
                    }
                }
            }
        }
    }

    fn into(
        self,
        self_id: &<Game<G> as CardGame>::UniqueId,
        players: &HashMap<<Game<G> as CardGame>::UniqueId, OtherPlayerId>,
    ) -> Result<InputMessageToken<G::Deck>, PlayerMessageError<UniqueId, G::Deck>> {
        use InputMessageToken::*;
        use PlayerMessageError::*;
        use Token::*;
        type Pub<T> = PublicMessageToken<T>;
        type Priv<T> = PrivateMessageToken<T>;
        type Hid = HiddenMessageToken;
        Ok(match self {
            PublicAttestedCard(card) => Public(Pub::AttestedCard(card)),
            PublicPlayer(p) => Public(Pub::Player(if &p == self_id {
                PlayerId::This
            } else {
                players
                    .get(&p)
                    .map(|id| id.clone().into())
                    .ok_or(UnknownPlayer(p))?
            })),
            PublicValue(v) => Public(Pub::Value(v)),
            PrivateAttestedCard(to, card) => {
                if &to == self_id {
                    Private(Priv::AttestedCard(card))
                } else {
                    let to = *players.get(&to).ok_or(UnknownPlayer(to))?;
                    Hidden(to, Hid::AttestedCard)
                }
            }
        })
    }
}

#[derive_where(Debug)]
pub enum GameMessage<G: GameType, C: CardGame>
where
    [(); G::Deck::SIZE]:,
{
    Message {
        from: C::UniqueId,
        message: PlayerMessage<G>,
    },
    Reveal {
        to: C::UniqueId,
        from: C::UniqueId,
        pos: DeckPosition<G::Deck>,
    },
}

impl<G: GameType, C: CardGame> Clone for GameMessage<G, C>
where
    [(); G::Deck::SIZE]:,
{
    fn clone(&self) -> Self {
        match self {
            Self::Message { from, message } => Self::Message {
                from: from.clone(),
                message: (*message).clone(),
            },
            Self::Reveal { to, from, pos } => Self::Reveal {
                to: to.clone(),
                from: from.clone(),
                pos: pos.clone(),
            },
        }
    }
}

#[derive(Debug)]
pub struct Bootstrap {
    id: u64,
    players: HashMap<u64, OtherPlayerId>,
}

#[derive(Debug)]
pub struct Game<G: GameType>
where
    [(); G::Deck::SIZE]:,
{
    id: u64,
    players: HashMap<u64, OtherPlayerId>,
    deck: [bool; G::Deck::SIZE],
    reveals: [HashSet<OtherPlayerId>; G::Deck::SIZE],
    shuffled_cards: [AttestedCard<G::Deck>; G::Deck::SIZE],
    _marker: PhantomData<G>,
}

type UniqueId = u64;

impl Bootstrap {
    pub fn new() -> Self {
        Self {
            id: rand::random(),
            players: HashMap::new(),
        }
    }

    pub fn id(&self) -> UniqueId {
        self.id
    }

    pub fn add_player(&mut self, id: UniqueId) -> Result<OtherPlayerId, OtherPlayerId> {
        let local_id: u8 = self.players.len().try_into().unwrap();
        let player_id = OtherPlayerId(local_id);
        match self.players.entry(id) {
            Entry::Occupied(o) => Err(*o.get()),
            Entry::Vacant(v) => {
                let _: &mut OtherPlayerId = v.insert(player_id);
                trace!("player {:?} added {:?} as {:?}", self.id, id, local_id);
                Ok(player_id)
            }
        }
    }

    pub fn start<G: GameType>(self) -> Game<G>
    where
        [(); G::Deck::SIZE]:,
    {
        let Self { id, players } = self;
        let sorted_keys = {
            let mut keys: Vec<_> = players.keys().chain(std::iter::once(&id)).collect();
            keys.sort_by(|a, b| a.cmp(b));
            keys
        };
        let mut hash = Sha256::new();
        for key in sorted_keys {
            hash.update(key.to_le_bytes());
        }
        let mut seed = <<rand::rngs::StdRng as rand::SeedableRng>::Seed as Default>::default();
        hash.finalize_into(generic_array::GenericArray::from_mut_slice(&mut seed));
        let mut rng = <rand::rngs::StdRng as rand::SeedableRng>::from_seed(seed);
        let mut shuffled_cards = Card::<G::Deck>::all().map(AttestedCard);
        shuffled_cards.shuffle(&mut rng);

        trace!("{:?} starting with players: {:?}", id, players);
        Game {
            id,
            players,
            reveals: [(); G::Deck::SIZE].map(|_| Default::default()),
            deck: [false; G::Deck::SIZE],
            shuffled_cards,
            _marker: PhantomData,
        }
    }
}

impl<'s> TurnOrderProvider for &'s Bootstrap {
    type Iter = impl Iterator<Item = PlayerId>;
    fn turn_order(self) -> Self::Iter {
        let mut players: Vec<_> = self
            .players
            .iter()
            .map(|(k, v)| (*k, v.clone().into()))
            .chain(std::iter::once((self.id, PlayerId::This)))
            .collect();
        players.sort_by(|(a, _), (b, _)| a.cmp(b));
        players.into_iter().map(|(_, id)| id)
    }
}

#[derive(Error)]
#[derive_where(Debug)]
pub enum GameOutputError<D: DeckType> {
    #[error("failed to encode token at {0}: {1:?}")]
    EncodeError(usize, PlayerMessageError<OtherPlayerId, D>),
}

#[derive(Error)]
#[derive_where(Debug)]
pub enum GameInputError<G: GameType>
where
    <G::Message as TryFrom<InputMessage<G::Deck>>>::Error: Debug + std::error::Error,
    [(); G::Deck::SIZE]:,
{
    #[error("received message from unknown player with ID {0}")]
    UnknownSender(<Game<G> as CardGame>::UniqueId),
    #[error("failed to decode token at {0}: {1}")]
    TranslateError(usize, PlayerMessageError<UniqueId, G::Deck>),
    #[error("failed to parse message: {0}")]
    ParseError(<G::Message as TryFrom<InputMessage<G::Deck>>>::Error),
}

trait InputMessageExt<'s, D: DeckType + 's> {
    type Iter: Iterator<Item = &'s AttestedCard<D>>;
    fn attested_cards(self) -> Self::Iter;
}

impl<'s, D: DeckType + 's> InputMessageExt<'s, D> for &'s InputMessage<D> {
    type Iter = impl Iterator<Item = &'s AttestedCard<D>>;

    fn attested_cards(self) -> Self::Iter {
        self.into_iter().filter_map(|t| match t {
            InputMessageToken::Public(t) => match t {
                PublicMessageToken::AttestedCard(card) => Some(card),
                PublicMessageToken::Player(_) => None,
                PublicMessageToken::Value(_) => None,
            },
            InputMessageToken::Private(t) => match t {
                PrivateMessageToken::AttestedCard(card) => Some(card),
            },
            InputMessageToken::Hidden(_, _) => None,
        })
    }
}

impl<G: GameType> CardGame for Game<G>
where
    G: Clone,
    [(); G::Deck::SIZE]:,
    <<G as GameType>::Message as TryFrom<InputMessage<G::Deck>>>::Error: Debug,
{
    type UniqueId = u64;
    type PlayerIter = std::collections::hash_set::IntoIter<OtherPlayerId>;
    type Game = G;

    type OutputError = GameOutputError<G::Deck>;

    type InputError = GameInputError<G>;
    type GameMessage = GameMessage<G, Self>;

    fn other_players(&self) -> Self::PlayerIter {
        self.players
            .values()
            .cloned()
            .collect::<HashSet<_>>()
            .into_iter()
    }

    fn send(&mut self, action: G::Action) -> Result<GameMessage<G, Self>, Self::OutputError> {
        let players = self.players.iter().map(|(k, v)| (*v, *k)).collect();
        info!("player {:?} is sending {:?}", self.id, &action);
        let message = PlayerMessage::new(action.into(), &self.id, &players, &self.deck)
            .map_err(|(i, e)| GameOutputError::EncodeError(i, e))?;
        Ok(GameMessage::Message {
            from: self.id,
            message: message,
        })
    }

    fn receive(
        &mut self,
        message: GameMessage<G, Self>,
    ) -> Result<Option<GameInput<G>>, Self::InputError> {
        match message {
            GameMessage::Message { from, message } => {
                trace!(
                    "{:?} received message from {:?}: {:?}",
                    self.id,
                    from,
                    message
                );
                let from = *self
                    .players
                    .get(&from)
                    .ok_or(GameInputError::UnknownSender(from))?;
                let message = match message.into(&self.id, &self.players) {
                    Ok(message) => message,
                    Err((i, e)) => {
                        warn!("{:?} decoding failed at {}: {:?}", self.id, i, e);
                        return Err(GameInputError::TranslateError(i, e));
                    }
                };
                let observations: Vec<AttestedCard<_>> =
                    message.attested_cards().cloned().collect();
                let message = <G::Message as TryFrom<InputMessage<G::Deck>>>::try_from(message)
                    .map_err(|e| GameInputError::ParseError(e))?;
                info!("player {:?} received {:?}", self.id, message);
                for card in observations {
                    self.deck[card.card()] = true;
                }
                Ok(Some(GameInput::Message(from, message)))
            }
            GameMessage::Reveal { to, from, pos } => {
                if to == self.id {
                    let from = *self
                        .players
                        .get(&from)
                        .ok_or(GameInputError::UnknownSender(from))?;
                    let reveals = &mut self.reveals[pos];
                    reveals.insert(from);
                    if reveals.len() == self.players.len() {
                        self.deck[self.shuffled_cards[pos].card()] = true;
                        Ok(Some(GameInput::Reveal(AttestedDeckCard(
                            pos,
                            self.shuffled_cards[pos],
                        ))))
                    } else {
                        Ok(None)
                    }
                } else {
                    Ok(None)
                }
            }
        }
    }

    fn reveal(&mut self, to: OtherPlayerId, pos: DeckPosition<G::Deck>) -> GameMessage<G, Self> {
        GameMessage::Reveal {
            to: self
                .players
                .iter()
                .find_map(|(k, v)| if v == &to { Some(*k) } else { None })
                .unwrap(),
            from: self.id,
            pos,
        }
    }

    type FinishInput = ();
    type FinishedGame = Finished<G>;
    type FinishError = Infallible;

    fn finish(self, _input: Self::FinishInput) -> Result<Self::FinishedGame, Self::FinishError> {
        Ok(Finished(
            self.players
                .into_iter()
                .map(|(k, v)| (k, v.into()))
                .chain([(self.id, PlayerId::This)])
                .collect(),
            self.shuffled_cards,
        ))
    }
}

impl<G: GameType> Game<G>
where
    [(); G::Deck::SIZE]:,
{
    #[cfg(test)]
    pub(crate) fn deck(&self) -> &[AttestedCard<G::Deck>; G::Deck::SIZE] {
        &self.shuffled_cards
    }
}

pub struct Finished<G: GameType>(
    HashMap<u64, PlayerId>,
    [AttestedCard<G::Deck>; G::Deck::SIZE],
)
where
    [(); G::Deck::SIZE]:;

impl<G> GameMessageDecoder<Game<G>> for Finished<G>
where
    G: LoggableGameType,
    <G::GameEvent as TryFrom<(PlayerId, PublicEvent<G::Deck>)>>::Error: std::fmt::Debug,
    [(); G::Deck::SIZE]:,
{
    type DecodeError = Infallible;
    fn decode_message(
        &self,
        message: &<Game<G> as CardGame>::GameMessage,
    ) -> Result<G::GameEvent, Infallible> {
        use RevealedMessageToken::*;
        type Pub<D> = PublicMessageToken<D>;
        type Priv<D> = PrivateMessageToken<D>;

        let Self(player_map, _) = self;
        let r = match message {
            GameMessage::Message { from, message } => {
                let tokens: RevealedMessage<_> = message
                    .0
                    .iter()
                    .map(|token| match token {
                        Token::PublicAttestedCard(card) => Public(Pub::AttestedCard(*card)),
                        Token::PublicPlayer(player) => Public(Pub::Player(player_map[player])),
                        Token::PublicValue(value) => Public(Pub::Value(*value)),
                        Token::PrivateAttestedCard(to, card) => {
                            Private(player_map[to], Priv::AttestedCard(*card))
                        }
                    })
                    .collect();
                (player_map[from], PublicEvent::Message(tokens))
            }
            GameMessage::Reveal { to, from, pos } => {
                (player_map[from], PublicEvent::Reveal(*pos, player_map[to]))
            }
        };
        Ok(<G::GameEvent as TryFrom<_>>::try_from(r).unwrap())
    }
}

impl<G: GameType> DeckProvider<G::Deck> for Finished<G>
where
    [(); G::Deck::SIZE]:,
{
    fn cards(&self) -> &[AttestedCard<G::Deck>; G::Deck::SIZE] {
        &self.1
    }
}

#[cfg(test)]
mod tests {
    use assert_matches::assert_matches;
    use unwrap_infallible::UnwrapInfallible as _;

    use crate::game::{
        log::{FinishedCardGame, FinishedLoggedGame, LoggingGame},
        testutil::FakeGameType,
        MapPlayer,
    };

    use super::*;
    type Out<D> = OutputMessageToken<D>;
    type In<D> = InputMessageToken<D>;
    type Pub<D> = PublicMessageToken<D>;
    type Priv<D> = PrivateMessageToken<D>;

    fn bootstrap_players<const N: usize>() -> [Game<FakeGameType>; N] {
        assert!(N > 1);
        let mut bootstraps: Vec<_> = (0..N).map(|_| Bootstrap::new()).collect();

        for i in 0..N {
            for j in 0..N {
                if i == j {
                    continue;
                }
                let id = bootstraps[i].id;
                bootstraps[j].add_player(id).unwrap();
            }
        }
        bootstraps
            .into_iter()
            .map(|b| b.start())
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    }

    #[test]
    fn send_game_message() {
        let [mut a, mut b] = bootstrap_players();
        let sent_message = [Out::Public(Pub::Value(2859))].into_iter().collect();
        let expected_input = [In::Public(Pub::Value(2859))].into_iter().collect();

        let message = assert_matches!(a.send(sent_message), Ok(message) => message);

        assert_matches!(
            b.receive(message),
            Ok(Some(GameInput::Message(OtherPlayerId(0), message))) => assert_eq!(message, expected_input)
        );
    }

    #[test]
    fn reveal_unknown_card_privately() {
        let [mut a, mut _b] = bootstrap_players();

        let message = [
            Out::Public(Pub::Value(32)),
            Out::Private(OtherPlayerId(0), Priv::AttestedCard(a.deck()[5])),
        ]
        .into_iter()
        .collect();
        assert_matches!(
            a.send(message),
            Err(GameOutputError::EncodeError(1, PlayerMessageError::UnknownCard(card))) => {
                assert_eq!(card, *a.deck()[5].card())
            }
        );
    }

    #[test]
    fn reveal_unknown_card_publicly() {
        let [mut a, mut _b] = bootstrap_players();

        let message = [
            Out::Public(Pub::Value(861)),
            Out::Public(Pub::Value(17495)),
            Out::Public(Pub::AttestedCard(a.deck()[5])),
        ]
        .into_iter()
        .collect();
        assert_matches!(
            a.send(message),
            Err(GameOutputError::EncodeError(2, PlayerMessageError::UnknownCard(card))) => {
                assert_eq!(card, *a.deck()[5].card())
            }
        );
    }

    #[test]
    fn send_attested_card_after_reveal() {
        let [mut a, mut b] = bootstrap_players();

        let message = a.reveal(OtherPlayerId(0), DeckPosition::new(5));
        let card = assert_matches!(b.receive(message), Ok(Some(GameInput::Reveal(card))) => card);
        assert_eq!(card, AttestedDeckCard(DeckPosition::new(5), a.deck()[5]));

        let message = [
            Out::Public(Pub::Value(32)),
            Out::Private(OtherPlayerId(0), Priv::AttestedCard(*card.card())),
        ]
        .into_iter()
        .collect();
        let encoded = assert_matches!(b.send(message), Ok(message) => message);
        let decoded = assert_matches!(a.receive(encoded), Ok(Some(decoded)) => decoded);
        assert_eq!(
            decoded,
            GameInput::Message(
                OtherPlayerId(0),
                [
                    In::Public(Pub::Value(32)),
                    In::Private(Priv::AttestedCard(*card.card()))
                ]
                .into_iter()
                .collect()
            )
        );
    }

    #[test]
    fn private_tokens_hidden() {
        let [mut a, mut b, mut c] = bootstrap_players();
        assert_matches!(
            a.receive(b.reveal(b.players[&a.id], DeckPosition::new(2))),
            Ok(None)
        );
        let card = assert_matches!(a.receive(c.reveal(c.players[&a.id], DeckPosition::new(2))), Ok(Some(GameInput::Reveal(card))) => card);

        let b_for_a = a.players[&b.id];

        let message = a
            .send(
                [
                    Out::Public(Pub::Value(23)),
                    Out::Private(b_for_a, Priv::AttestedCard(*card.card())),
                ]
                .into_iter()
                .collect(),
            )
            .unwrap();
        let b_input = assert_matches!(b.receive(message.clone()), Ok(Some(GameInput::Message(_, input))) => input);
        let c_input =
            assert_matches!(c.receive(message),  Ok(Some(GameInput::Message(_, input))) => input);

        assert_eq!(
            b_input,
            [
                In::Public(Pub::Value(23)),
                In::Private(Priv::AttestedCard(*card.card()))
            ]
            .into_iter()
            .collect()
        );

        assert_eq!(
            c_input,
            [
                In::Public(Pub::Value(23)),
                In::Hidden(c.players[&b.id], HiddenMessageToken::AttestedCard),
            ]
            .into_iter()
            .collect()
        );
    }

    #[test]
    fn finish_game() {
        let [mut a, mut b] = bootstrap_players().map(LoggingGame::new);
        let reveal = b.reveal(OtherPlayerId(0), DeckPosition::new(0));
        let card = assert_matches!(a.receive(reveal), Ok(Some(GameInput::Reveal(card))) => card);

        let message = assert_matches!(a.send(
            [
                Out::Public(Pub::Value(90348)),
                Out::Private(OtherPlayerId(0), Priv::AttestedCard(*card.card())),
            ]
            .into_iter()
            .collect(),
        ), Ok(message) => message);
        b.receive(message).unwrap();

        let [a, b]: [FinishedLoggedGame<Game<FakeGameType>>; 2] = [
            a.finish(()).unwrap_infallible(),
            b.finish(()).unwrap_infallible(),
        ];

        let a_players: HashMap<_, _> = [
            (PlayerId::This, 'A'),
            (PlayerId::Other(OtherPlayerId(0)), 'B'),
        ]
        .into();

        let b_players: HashMap<_, _> = [
            (PlayerId::This, 'B'),
            (PlayerId::Other(OtherPlayerId(0)), 'A'),
        ]
        .into();

        assert_eq!(
            a.log()
                .map(|m| m.map(|m| m.map_player(&a_players)))
                .collect::<Vec<_>>(),
            b.log()
                .map(|m| m.map(|m| m.map_player(&b_players)))
                .collect::<Vec<_>>(),
        );
    }
}
