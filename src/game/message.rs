use std::{hash::Hash, ops::Index};

extern crate derive_more;
use derive_where::derive_where;

use crate::*;

use super::AttestedCard;

/// A token that, when it appears in a message, is visible to all players.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PublicMessageToken<D: DeckType, P = PlayerId> {
    /// Encodes the value of a card and any information required to prove to
    /// other players that the value was previously revealed to the local
    /// player.
    AttestedCard(AttestedCard<D>),
    /// Encodes an unambiguous reference to one of the players.
    Player(P),
    /// Encodes a fixed-size value.
    ///
    /// Clients can use this to include arbitrary information in a message,
    /// including unattested references to card values or deck positions. The
    /// interpretation of each `Value` token is left up to the client.
    Value(u64),
}

/// A token that, when it appears in a message, is visible to only one player.
#[derive_where(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrivateMessageToken<D: DeckType> {
    /// Encodes the value of a card and any information requried to prove to
    /// other players that the value was previously revealed to the local
    /// player.
    AttestedCard(AttestedCard<D>),
}

/// A single output token in a message.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum OutputMessageToken<D: DeckType> {
    /// Encodes a message token visible to all players.
    Public(PublicMessageToken<D>),
    /// Encodes a message token visible only to the specified player.
    Private(OtherPlayerId, PrivateMessageToken<D>),
}

/// A complete output message.
///
/// All client game messages must be representable as a sequence of
/// [`OutputMessageToken`]s, ecapsulated as an `OutputMessage`.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct OutputMessage<D: DeckType>(Vec<OutputMessageToken<D>>);

/// A token indicating what type of information was hidden but not the value.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum HiddenMessageToken {
    /// Indicates that the value of a card was provided to some player.
    AttestedCard,
}

/// A single token in a received message.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum InputMessageToken<D: DeckType> {
    /// A value included by the sending player visible to all players.
    Public(PublicMessageToken<D>),
    /// A value included by the sending player visible only to the local player.
    Private(PrivateMessageToken<D>),
    /// Indicates a value included by the sending player visible only to the
    /// specified player.
    Hidden(OtherPlayerId, HiddenMessageToken),
}

/// A complete transcription of an input message.
///
/// When a client sends an [`OutputMessage`], it is transformed into an
/// [`InputMessage`] by the receiving game.  [`OutputMessageToken::Public`]
/// values are copied identically, while [`OutputMessageToken::Private`] values
/// are transcribed as either [`InputMessageToken::Private`], if the local
/// player was specified as the recipient in the output, or
/// [`InputMessageToken::Hidden`] values if the token was addressed to another
/// player.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InputMessage<D: DeckType>(Vec<InputMessageToken<D>>);

/// A single token in a public-only message.
///
/// Instances of this class are constructed when the contents of an
/// [`OutputMessage`] are de-obfuscated, as at the end of a game. The values
/// correspond exactly to those of [`OutputMessageToken`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RevealedMessageToken<D: DeckType, P = PlayerId> {
    /// Indicates a public token in the original output message.
    Public(PublicMessageToken<D, P>),
    /// Indicates a token in the output message that was visible only to the
    /// specified player, but which is now revealed.
    Private(P, PrivateMessageToken<D>),
}

/// A complete public-only message.
///
/// Instances of this class are constructed by de-obfuscating an
/// [`OutputMessage`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RevealedMessage<D: DeckType, P = PlayerId>(Vec<RevealedMessageToken<D, P>>);

/// A de-obfuscated event in the public log.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PublicEvent<D: DeckType, P = PlayerId> {
    // Records that a message was sent.
    Message(RevealedMessage<D, P>),
    /// Records an attempt to reveal the card at the given position to the
    /// receiving player.
    Reveal(DeckPosition<D>, P),
}

/// Convenience trait for converting from one player ID representation to another.
///
/// This can be used to, for example, convert from relative player IDs to
/// absolute for purposes of comparing or persisting messages.
pub trait MapPlayer<P: Hash + Eq + Copy> {
    /// The type of the output after converting from the input ID type `P` to the output type `I`.
    type Mapped<I>;

    /// Converts and returns the output.
    ///
    /// Converts all instances of `P` into `I` using the conversion specified by `players`.
    fn map_player<'p, I: Clone, M: Index<&'p P, Output = I>>(
        &'p self,
        players: &'p M,
    ) -> Self::Mapped<I>
    where
        P: 'p;
}

impl<D: DeckType, P: Hash + Eq + Copy> MapPlayer<P> for PublicEvent<D, P> {
    type Mapped<I> = PublicEvent<D, I>;

    fn map_player<'p, I: Clone, M: Index<&'p P, Output = I>>(
        &'p self,
        players: &'p M,
    ) -> Self::Mapped<I>
    where
        P: 'p,
    {
        match self {
            PublicEvent::Message(m) => PublicEvent::Message(m.map_player(players)),
            PublicEvent::Reveal(pos, p) => PublicEvent::Reveal(*pos, players[p].clone()),
        }
    }
}

impl<D: DeckType, P: Hash + Eq + Copy> MapPlayer<P> for RevealedMessage<D, P> {
    type Mapped<I> = RevealedMessage<D, I>;

    fn map_player<'p, I: Clone, M: Index<&'p P, Output = I>>(
        &'p self,
        players: &'p M,
    ) -> Self::Mapped<I>
    where
        P: 'p,
    {
        self.into_iter()
            .map(|t| match t {
                RevealedMessageToken::Public(p) => {
                    RevealedMessageToken::Public(p.map_player(players))
                }
                RevealedMessageToken::Private(p, t) => {
                    RevealedMessageToken::Private(players[p].clone(), *t)
                }
            })
            .collect()
    }
}

impl<D: DeckType, P: Hash + Eq + Copy> MapPlayer<P> for PublicMessageToken<D, P> {
    type Mapped<I> = PublicMessageToken<D, I>;

    fn map_player<'p, I: Clone, M: Index<&'p P, Output = I>>(
        &'p self,
        players: &'p M,
    ) -> Self::Mapped<I> {
        match self {
            PublicMessageToken::Player(p) => PublicMessageToken::Player(players[p].clone()),
            PublicMessageToken::AttestedCard(c) => PublicMessageToken::AttestedCard(*c),
            PublicMessageToken::Value(v) => PublicMessageToken::Value(*v),
        }
    }
}

impl<P: Hash + Eq + Copy, T: MapPlayer<P>> MapPlayer<P> for (P, T) {
    type Mapped<I> = (I, T::Mapped<I>);

    fn map_player<'p, I: Clone, M: Index<&'p P, Output = I>>(
        &'p self,
        players: &'p M,
    ) -> Self::Mapped<I> {
        let (p, t) = self;
        (players[p].clone(), t.map_player(players))
    }
}

impl<D: DeckType> IntoIterator for OutputMessage<D> {
    type Item = OutputMessageToken<D>;
    type IntoIter = <Vec<Self::Item> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'a, D: DeckType> IntoIterator for &'a OutputMessage<D> {
    type Item = &'a OutputMessageToken<D>;
    type IntoIter = <&'a Vec<OutputMessageToken<D>> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<D: DeckType> FromIterator<OutputMessageToken<D>> for OutputMessage<D> {
    fn from_iter<T: IntoIterator<Item = OutputMessageToken<D>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<D: DeckType> FromIterator<InputMessageToken<D>> for InputMessage<D> {
    fn from_iter<T: IntoIterator<Item = InputMessageToken<D>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<D: DeckType> IntoIterator for InputMessage<D> {
    type Item = InputMessageToken<D>;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'s, D: DeckType> IntoIterator for &'s InputMessage<D> {
    type Item = &'s InputMessageToken<D>;
    type IntoIter = impl Iterator<Item = Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl<D: DeckType, P> FromIterator<RevealedMessageToken<D, P>> for RevealedMessage<D, P> {
    fn from_iter<T: IntoIterator<Item = RevealedMessageToken<D, P>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<D: DeckType, P> From<RevealedMessage<D, P>> for PublicEvent<D, P> {
    fn from(message: RevealedMessage<D, P>) -> Self {
        Self::Message(message)
    }
}

impl<D: DeckType, P> IntoIterator for RevealedMessage<D, P> {
    type Item = RevealedMessageToken<D, P>;
    type IntoIter = std::vec::IntoIter<Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

impl<'s, D: DeckType, P> IntoIterator for &'s RevealedMessage<D, P> {
    type Item = &'s RevealedMessageToken<D, P>;
    type IntoIter = impl Iterator<Item = Self::Item>;
    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}
