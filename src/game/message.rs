extern crate derive_more;
use crate::*;

use super::AttestedCard;

/// A token that, when it appears in a message, is visible to all players.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PublicMessageToken<D: DeckType> {
    /// Encodes the value of a card and any information required to prove to
    /// other players that the value was previously revealed to the local
    /// player.
    AttestedCard(AttestedCard<D>),
    /// Encodes an unambiguous reference to one of the players.
    Player(PlayerId),
    /// Encodes a fixed-size value.
    ///
    /// Clients can use this to include arbitrary information in a message,
    /// including unattested references to card values or deck positions. The
    /// interpretation of each `Value` token is left up to the client.
    Value(u64),
}

/// A token that, when it appears in a message, is visible to only one player.
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
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
pub enum RevealedMessageToken<D: DeckType> {
    /// Indicates a public token in the original output message.
    Public(PublicMessageToken<D>),
    /// Indicates a token in the output message that was visible only to the
    /// specified player, but which is now revealed.
    Private(PlayerId, PrivateMessageToken<D>),
}

/// A complete public-only message.
///
/// Instances of this class are constructed by de-obfuscating an
/// [`OutputMessage`].
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RevealedMessage<D: DeckType>(Vec<RevealedMessageToken<D>>);

/// A de-obfuscated event in the public log.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum PublicEvent<D: DeckType> {
    // Records that a message was sent.
    Message(RevealedMessage<D>),
    /// Records an attempt to reveal the card at the given position to the
    /// receiving player.
    Reveal(DeckPosition<D>, PlayerId),
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

impl<D: DeckType> FromIterator<RevealedMessageToken<D>> for RevealedMessage<D> {
    fn from_iter<T: IntoIterator<Item = RevealedMessageToken<D>>>(iter: T) -> Self {
        Self(iter.into_iter().collect())
    }
}

impl<D: DeckType> From<RevealedMessage<D>> for PublicEvent<D> {
    fn from(message: RevealedMessage<D>) -> Self {
        Self::Message(message)
    }
}
