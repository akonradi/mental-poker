pub mod log;
pub mod message;

use std::fmt::Debug;

use crate::deck::*;
use crate::*;
pub use message::*;

/// A card whose value is known and unforgeable.
///
/// Instances of this type are only created by implementations of [`CardGame`].
/// Where [`Card<D>`] allows one to name a specific card in the deck, an
/// instance of `AttestedCard<D>` can only be provided if the value of the card
/// was revealed by players.
#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct AttestedCard<D>(Card<D>);

/// A card whose value and position in a shuffled deck are known and unforgeable.
/// Instances of this type are only created by implementations of [`CardGame`].
/// Like [`AttestedCard<D>`], `AttestedDeckCard<D>` represents a revealed value,
/// though it also includes the position in the shuffled deck that holds the value.
#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub struct AttestedDeckCard<D>(DeckPosition<D>, AttestedCard<D>);

impl<D: DeckType> AttestedCard<D> {
    /// Returns the value of the attested card.
    pub fn card(&self) -> &Card<D> {
        &self.0
    }
}

impl<D: DeckType> AttestedDeckCard<D> {
    /// Returns the position from which the value was drawn.
    pub fn position(&self) -> &DeckPosition<D> {
        &self.0
    }

    /// Returns the attested card value.
    ///
    /// This can be used to prove that a card value is known without revealing
    /// from where.
    pub fn card(&self) -> &AttestedCard<D> {
        &self.1
    }
}

impl<D: DeckType> From<AttestedDeckCard<D>> for AttestedCard<D> {
    fn from(c: AttestedDeckCard<D>) -> Self {
        c.1
    }
}

impl<'a, T, D: DeckType> std::ops::Index<&'a Card<D>> for [T; D::SIZE] {
    type Output = T;

    fn index(&self, index: &'a Card<D>) -> &Self::Output {
        std::ops::Index::<usize>::index(self, u16::from(index) as usize)
    }
}

impl<'a, T, D: DeckType> std::ops::IndexMut<&'a Card<D>> for [T; D::SIZE] {
    fn index_mut(&mut self, index: &'a Card<D>) -> &mut Self::Output {
        std::ops::IndexMut::<usize>::index_mut(self, u16::from(index) as usize)
    }
}

/// A message emitted by a [`CardGame`] implementation.
///
/// In the event-based game model, a `GameInput` is returned in response to the
/// game instance receiving a message from a peer. It represents decoded "input"
/// to the client of the game library.
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum GameInput<G: GameType> {
    /// A message was received from the given player.
    Message(OtherPlayerId, G::Message),
    /// A card was revealed to the local player by the other players.
    Reveal(AttestedDeckCard<G::Deck>),
}

/// Provides the turn order for a set of players.
pub trait TurnOrderProvider {
    type Iter: Iterator<Item = PlayerId>;

    /// Returns an iterator over the players that yields them in turn order.
    fn turn_order(&self) -> Self::Iter;
}

pub trait GameType: Clone + Debug + Eq + PartialEq + PartialOrd + PartialEq + 'static {
    type Deck: DeckType + 'static;
    type Action: Into<OutputMessage<Self::Deck>> + Debug;
    type MessageParseError: Debug;
    type Message: TryFrom<InputMessage<Self::Deck>, Error = Self::MessageParseError> + Debug;
}

/// An event-based interface used by clients to implement a card game.
///
/// This trait defines the behavior that clients of this library can build on to
/// implement a fully-featured card game. The interface presented by this trait
/// is completely synchronous. Clients broadcast messages to other instances
/// using [`CardGame::send`] and [`CardGame::reveal`], and receive events using
/// [`CardGame::receive`].  Implementers of `CardGame` are not responsible for
/// providing any sort of inter-object or inter-process communication; instead,
/// users are responsible for broadcasting any outputs of type
/// [`CardGame::GameMessage`] to all other peers, and for providing all received
/// inputs via `CardGame::receive`.
///
/// In response to inputs, `CardGame::receive` will provide any decoded game
/// events intended for the local client. The client is expected to handle these
/// outputs according to its own logic.
pub trait CardGame {
    /// The type of the game being played.
    type Game: GameType;

    /// The type of a player identifier.
    ///
    /// This is an absolute identifier, as opposed to [`PlayerId`] instances,
    /// which have value only when interfacing with the local `CardGame`
    /// instance.
    type UniqueId: Clone + Copy + Eq + PartialEq + std::hash::Hash + Debug;

    type OutputError: std::fmt::Debug + std::error::Error;

    type InputError: std::fmt::Debug + std::error::Error;

    /// The type of messages exchanged between peer instances of this type.
    type GameMessage: std::fmt::Debug + Clone;

    type PlayerIter: Iterator<Item = OtherPlayerId> + ExactSizeIterator;

    /// Returns the set of other players as an iterator.
    fn other_players(&self) -> Self::PlayerIter;

    /// Called by the game client to send a message to other clients.
    ///
    /// Returns the encoded message to convey to all peer clients, or an error.
    fn send(
        &mut self,
        action: <Self::Game as GameType>::Action,
    ) -> Result<Self::GameMessage, Self::OutputError>;

    /// Called by the game client when a new messsage is received from other clients.
    ///
    /// Returns input from a peer client, or any procesing error that arises.
    /// Not all accepted inputs produce an output, in which case `Ok(None)` is
    /// returned.
    fn receive(
        &mut self,
        message: Self::GameMessage,
    ) -> Result<Option<GameInput<Self::Game>>, Self::InputError>;

    /// Allow a given player to see the card at the specified position.
    ///
    /// When all peer instances have sent reveal messages (the output of this
    /// method) for the same deck position addressed to the same player, that
    /// player is able to decode the card at that position.
    fn reveal(
        &mut self,
        to: OtherPlayerId,
        pos: DeckPosition<<Self::Game as GameType>::Deck>,
    ) -> Self::GameMessage;

    type FinishInput: std::default::Default;
    type FinishedGame;
    type FinishError: std::error::Error + std::fmt::Debug + 'static;

    /// Attempt to complete the game.
    fn finish(self, input: Self::FinishInput) -> Result<Self::FinishedGame, Self::FinishError>;
}

#[cfg(test)]
pub mod testutil {
    use std::convert::Infallible;

    use super::*;
    use crate::deck::testutil::FakeDeck;

    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
    pub enum FakeGameType {}

    impl GameType for FakeGameType {
        type Deck = FakeDeck;
        type Action = OutputMessage<Self::Deck>;
        type MessageParseError = Infallible;
        type Message = InputMessage<Self::Deck>;
    }
}
