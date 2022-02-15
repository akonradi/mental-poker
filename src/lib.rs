#![allow(incomplete_features)]
#![feature(step_trait)]
#![feature(generic_const_exprs)]
#![feature(generic_associated_types)]
#![feature(type_alias_impl_trait)]
#![cfg_attr(test, feature(generic_arg_infer))]

pub(crate) mod deck;
pub mod game;

use derive_more::From;

pub use deck::{Card, DeckPosition, DeckType};

/// An identifier used to uniquely identify another player.
///
/// This identifier is locally scoped, and meaningful only in the context of the
/// [`game::CardGame`] instance that creates it.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct OtherPlayerId(u8);

impl OtherPlayerId {
    /// Constructs a new ID.
    #[cfg(test)]
    pub(crate) fn new(id: u8) -> Self {
        Self(id)
    }
}

/// An identifier use to uniquely identify a player.
///
/// This identifier is locally scoped, and meaningful only in the context of the
/// [`game::CardGame`] instance that creates it.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash, From)]
pub enum PlayerId {
    /// The "local" player.
    This,
    /// Some other player.
    Other(OtherPlayerId),
}

impl From<&'_ OtherPlayerId> for u8 {
    fn from(id: &OtherPlayerId) -> u8 {
        id.0
    }
}

impl From<OtherPlayerId> for u8 {
    fn from(id: OtherPlayerId) -> u8 {
        id.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn other_player_id_unique() {
        let a = OtherPlayerId::new(8);
        let b = OtherPlayerId::new(91);
        assert_ne!(a, b);
    }

    #[test]
    fn other_player_id_into() {
        assert_eq!(5u8, OtherPlayerId::new(5).into());
        assert_eq!(5u8, (&OtherPlayerId::new(5)).into());
    }

    #[test]
    fn player_id_unique() {
        let a = OtherPlayerId::new(8).into();
        let b = OtherPlayerId::new(91).into();
        let t = PlayerId::This;
        assert_ne!(t, a);
        assert_ne!(t, b);
        assert_ne!(a, b);
    }
}
