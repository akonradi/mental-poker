use crate::{DeckPosition, DeckType};

#[derive(Debug, Clone)]
pub struct DrawPile<D> {
    cards: std::ops::RangeInclusive<DeckPosition<D>>,
}

impl<D: DeckType> DrawPile<D> {
    pub fn new() -> Self {
        Self {
            cards: DeckPosition::positions(),
        }
    }

    pub fn top(&self) -> Option<&DeckPosition<D>> {
        if self.cards.is_empty() {
            return None;
        }
        Some(self.cards.start())
    }
}

impl<D: DeckType> Iterator for DrawPile<D> {
    type Item = DeckPosition<D>;
    fn next(&mut self) -> Option<Self::Item> {
        self.cards.next()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.cards.size_hint()
    }
}

impl<D: DeckType> ExactSizeIterator for DrawPile<D> {}
