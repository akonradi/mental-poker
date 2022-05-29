use std::str::FromStr;

use mental_poker::{game::AttestedCard, game::AttestedDeckCard, Card, DeckType};

#[derive(Debug, Eq, PartialEq, Hash, Clone, Copy, PartialOrd, Ord)]
pub(crate) enum GoFishDeck {}

impl DeckType for GoFishDeck {
    const SIZE: usize = 52;
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug)]
pub(crate) enum Suit {
    A,
    B,
    C,
    D,
}

#[derive(Clone, Copy, Eq, PartialEq, Hash, Debug, PartialOrd, Ord)]
pub(crate) struct Rank(u8);

impl Rank {
    const MAX: u8 = 12;
    pub fn all() -> impl Iterator<Item = Rank> {
        (0..=Self::MAX).map(Rank)
    }

    #[cfg(test)]
    pub const fn test_new(rank: u8) -> Self {
        assert!(rank <= Self::MAX);
        Self(rank)
    }
}

#[derive(Debug)]
pub(crate) enum RankParseError {
    Parse(<u8 as FromStr>::Err),
    OutOfBounds(u8),
}

#[derive(PartialEq, Eq, Hash, Debug, Default, Copy, Clone)]
pub(crate) struct UnknownCard;

#[derive(PartialEq, Eq, Hash)]
pub(crate) struct SomeRank;

#[derive(PartialEq, Eq, Hash)]
pub(crate) struct SomeValue;

#[derive(Copy, Clone, PartialEq, Eq, std::hash::Hash)]
pub(crate) struct GoFishCard(Card<GoFishDeck>);

#[derive(Copy, Clone, PartialEq, Eq, std::hash::Hash)]
pub(crate) struct AttestedGoFishCard(AttestedCard<GoFishDeck>);

impl FromStr for Rank {
    type Err = RankParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u8::from_str(s)
            .map_err(RankParseError::Parse)
            .and_then(|u| u.try_into().map_err(RankParseError::OutOfBounds))
    }
}

impl std::fmt::Display for Rank {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("rank {}", self.0))
    }
}

impl std::error::Error for Rank {}

impl TryFrom<u8> for Rank {
    type Error = u8;
    fn try_from(v: u8) -> Result<Self, Self::Error> {
        if v <= Self::MAX {
            Ok(Self(v.try_into().unwrap()))
        } else {
            Err(v)
        }
    }
}

impl TryFrom<u8> for Suit {
    type Error = u8;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Suit::A),
            1 => Ok(Suit::B),
            2 => Ok(Suit::C),
            3 => Ok(Suit::D),
            x => Err(x),
        }
    }
}

impl From<Rank> for u8 {
    fn from(r: Rank) -> u8 {
        r.0
    }
}

impl std::fmt::Debug for GoFishCard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("GoFishCard")
            .field(&self.rank())
            .field(&self.suit())
            .finish()
    }
}

impl std::fmt::Debug for AttestedGoFishCard {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("Attested({:?})", GoFishCard(*self.0.card())))
    }
}

pub(crate) trait GoFishDeckCard {
    fn rank(&self) -> Rank;
    fn suit(&self) -> Suit;
}

impl GoFishDeckCard for GoFishCard {
    fn rank(&self) -> Rank {
        self.0.rank()
    }

    fn suit(&self) -> Suit {
        self.0.suit()
    }
}

impl GoFishDeckCard for Card<GoFishDeck> {
    fn rank(&self) -> Rank {
        (self.index() as u8 % (Rank::MAX + 1)).try_into().unwrap()
    }

    fn suit(&self) -> Suit {
        (self.index() as u8 / (Rank::MAX + 1)).try_into().unwrap()
    }
}

impl GoFishDeckCard for AttestedGoFishCard {
    fn rank(&self) -> Rank {
        self.0.card().rank()
    }

    fn suit(&self) -> Suit {
        self.0.card().suit()
    }
}

impl From<GoFishCard> for Card<GoFishDeck> {
    fn from(c: GoFishCard) -> Self {
        c.0
    }
}

impl<'a> From<&'a AttestedGoFishCard> for &'a AttestedCard<GoFishDeck> {
    fn from(c: &'a AttestedGoFishCard) -> &'a AttestedCard<GoFishDeck> {
        &c.0
    }
}

impl Into<AttestedCard<GoFishDeck>> for AttestedGoFishCard {
    fn into(self) -> AttestedCard<GoFishDeck> {
        self.0
    }
}

impl From<Card<GoFishDeck>> for GoFishCard {
    fn from(card: Card<GoFishDeck>) -> Self {
        Self(card)
    }
}

impl From<AttestedCard<GoFishDeck>> for AttestedGoFishCard {
    fn from(card: AttestedCard<GoFishDeck>) -> Self {
        Self(card)
    }
}

impl From<AttestedDeckCard<GoFishDeck>> for AttestedGoFishCard {
    fn from(card: AttestedDeckCard<GoFishDeck>) -> Self {
        Self(card.into())
    }
}

impl AsRef<Card<GoFishDeck>> for GoFishCard {
    fn as_ref(&self) -> &Card<GoFishDeck> {
        &self.0
    }
}

impl AsRef<AttestedCard<GoFishDeck>> for AttestedGoFishCard {
    fn as_ref(&self) -> &AttestedCard<GoFishDeck> {
        &self.0
    }
}

impl From<Rank> for SomeRank {
    fn from(_: Rank) -> Self {
        Self {}
    }
}

impl<D> From<Card<D>> for SomeValue {
    fn from(_: Card<D>) -> Self {
        Self {}
    }
}
