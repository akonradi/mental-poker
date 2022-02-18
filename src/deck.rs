use std::marker::PhantomData;

/// The type of a deck.
///
/// Implementations of this trait define the type of a deck. A deck has `SIZE`
/// cards, all unique. Types that implement `DeckType` do not need to be
/// instantiatable; they are used only as markers.
pub trait DeckType:
    std::fmt::Debug + Copy + Clone + Eq + PartialEq + PartialOrd + std::hash::Hash
{
    const SIZE: usize;
}

/// The value of a card in a deck.
///
/// A `Card<D>` represents the unique value of a card that is a member of `D:
/// DeckType`. Cards within a deck have values in the range [0, D::SIZE-1].
/// Custom traits can be used to attach additional information to a card, like
/// "suit" or "rank", derived from the card's index.
#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct Card<DeckType>(u16, PhantomData<DeckType>);

/// A position within a shuffled deck of cards.
///
/// A `DeckPosition<D>` represents an index into a shuffled list of cards who
/// are members of a deck `D : DeckType`.
#[derive(derivative::Derivative)]
#[derivative(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Hash)]
pub struct DeckPosition<DeckType>(usize, PhantomData<DeckType>);

impl<D: DeckType> Card<D> {
    /// Creates a new card with the given value.
    pub(crate) fn new(index: u16) -> Self {
        Self(index, PhantomData)
    }

    /// Returns the set of cards in the deck in order by value.
    pub fn all() -> [Self; D::SIZE]
    where
        [(); D::SIZE]:,
    {
        let mut cards = [Self::new(0); D::SIZE];
        for (i, c) in cards.iter_mut().enumerate() {
            c.0 = i.try_into().unwrap();
        }
        cards
    }

    pub fn index(&self) -> usize {
        self.0.into()
    }
}

impl<D: DeckType> DeckPosition<D> {
    const MAX: Self = DeckPosition(D::SIZE - 1, PhantomData);

    #[cfg(test)]
    pub(crate) fn new(index: usize) -> Self {
        assert!(index <= Self::MAX.into());
        Self(index, PhantomData)
    }

    /// Returns the position of the first card in the shuffled deck.
    pub fn start() -> Self {
        Self(0, PhantomData)
    }

    /// Returns a range over all positions in the shuffled deck.
    pub fn positions() -> std::ops::RangeInclusive<Self> {
        Self::start()..=Self::MAX
    }

    pub fn index(&self) -> usize {
        self.0.into()
    }
}

impl<D: DeckType> From<Card<D>> for u16 {
    fn from(c: Card<D>) -> u16 {
        c.0
    }
}

impl<D: DeckType> From<&'_ Card<D>> for u16 {
    fn from(c: &Card<D>) -> u16 {
        c.0
    }
}

impl<D> From<DeckPosition<D>> for usize {
    fn from(pos: DeckPosition<D>) -> Self {
        pos.0
    }
}

impl<D> From<&'_ DeckPosition<D>> for usize {
    fn from(pos: &DeckPosition<D>) -> Self {
        pos.0
    }
}

impl<D: DeckType> std::iter::Step for DeckPosition<D> {
    fn steps_between(start: &Self, end: &Self) -> Option<usize> {
        usize::steps_between(&start.0, &end.0)
    }

    fn forward_checked(start: Self, count: usize) -> Option<Self> {
        usize::forward_checked(start.0, count).and_then(|index| {
            if index < D::SIZE {
                Some(Self(index, PhantomData))
            } else {
                None
            }
        })
    }

    fn backward_checked(start: Self, count: usize) -> Option<Self> {
        usize::backward_checked(start.0, count).and_then(|index| {
            if index < D::SIZE {
                Some(Self(index, PhantomData))
            } else {
                None
            }
        })
    }

    fn forward(start: Self, count: usize) -> Self {
        Self::forward_checked(start, count).unwrap()
    }

    unsafe fn forward_unchecked(start: Self, count: usize) -> Self {
        Self(start.0 + count, PhantomData)
    }

    fn backward(start: Self, count: usize) -> Self {
        Self::backward_checked(start, count).unwrap()
    }

    unsafe fn backward_unchecked(start: Self, count: usize) -> Self {
        Self(start.0 - count, PhantomData)
    }
}

impl<T, D: DeckType> std::ops::Index<DeckPosition<D>> for [T; D::SIZE] {
    type Output = T;
    fn index(&self, position: DeckPosition<D>) -> &Self::Output {
        &self[position.0]
    }
}

impl<T, D: DeckType> std::ops::IndexMut<DeckPosition<D>> for [T; D::SIZE] {
    fn index_mut(&mut self, position: DeckPosition<D>) -> &mut Self::Output {
        &mut self[position.0]
    }
}

impl<D> std::fmt::Debug for Card<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("Card").field(&self.0).finish()
    }
}

impl<D> std::fmt::Debug for DeckPosition<D> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_tuple("DeckPosition").field(&self.0).finish()
    }
}

#[cfg(test)]
pub mod testutil {
    use super::*;

    #[derive(Hash, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
    pub enum FakeDeck {}

    impl DeckType for FakeDeck {
        const SIZE: usize = 10;
    }
}
