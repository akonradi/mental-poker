use std::collections::HashSet;

use log::info;

use crate::{
    cards::Rank,
    game::{DealState, GameState, Hand, TurnState},
};
use mental_poker::{
    game::CardGame,
    PlayerId::{self, *},
};

use crate::{
    cards::UnknownCard,
    cards::{AttestedGoFishCard, GoFishDeckCard},
    game::GoFishState,
    message::GoFishAction,
};

use super::{PlayerImpl, PlayerAction, PlayerActionError};

pub(crate) struct AutoPlayer {
    hand: Hand<AttestedGoFishCard>,
    on_request: Box<dyn FnMut(&mut Hand<AttestedGoFishCard>, &Rank) -> Vec<AttestedGoFishCard>>,
}

impl<G: CardGame> PlayerImpl<G> for AutoPlayer {
    fn action(&mut self, state: &mut GoFishState<UnknownCard>) -> Option<PlayerAction> {
        use DealState::*;
        use GameState::*;
        use TurnState::*;

        assert_eq!(state.player_hand(&PlayerId::This).len(), self.hand.len());
        match state.state() {
            Dealing(PlayerId::This, Starting) => {
                assert!(self.hand.len() <= state.deal_size());
                if self.hand.len() < state.deal_size() {
                    info!("self dealing");
                    Some(GoFishAction::Draw.into())
                } else {
                    Some(
                        GoFishAction::Reveal(
                            self.hand.take_pairs().into_iter().flatten().collect(),
                        )
                        .into(),
                    )
                }
            }
            Dealing(PlayerId::This, Drawing) => {
                if self.hand.len() == state.deal_size() {
                    Some(
                        GoFishAction::Reveal(
                            self.hand.take_pairs().into_iter().flatten().collect(),
                        )
                        .into(),
                    )
                } else {
                    None
                }
            }
            Dealing(Other(o), Drawing) => {
                Some(PlayerAction::RevealCard(*o, *state.top_of_deck().unwrap()))
            }
            Dealing(Other(_), Starting) => None,
            Running(_, Playing)
                if state.top_of_deck() == None && {
                    state.players().all(|(_, p)| p.hand().len() == 0)
                } =>
            {
                Some(PlayerAction::EndGame)
            }
            Running(PlayerId::This, Playing) => {
                let pairs = self.hand.take_pairs();
                if !pairs.is_empty() {
                    Some(GoFishAction::Reveal(pairs.into_iter().flatten().collect()).into())
                } else if self.hand.len() != 0 {
                    let mut with_cards = state.players().filter_map(|(id, p)| match id {
                        Other(o) => {
                            if p.hand().len() > 0 {
                                Some(*o)
                            } else {
                                None
                            }
                        }
                        PlayerId::This => None,
                    });
                    let to = with_cards.next().unwrap_or_else(|| {
                        state
                            .players()
                            .filter_map(|(id, _)| match id {
                                Other(o) => Some(*o),
                                PlayerId::This => None,
                            })
                            .next()
                            .unwrap()
                    });
                    let rank = self
                        .hand
                        .iter()
                        .map(|c| GoFishDeckCard::rank(c))
                        .collect::<HashSet<_>>()
                        .into_iter()
                        .next()
                        .unwrap();
                    Some(GoFishAction::Request { to, rank }.into())
                } else if state.top_of_deck().is_some() {
                    info!("self playing, deck not empty");
                    Some(GoFishAction::Draw.into())
                } else if state.players().all(|(_, p)| p.hand().len() == 0) {
                    Some(PlayerAction::EndGame)
                } else {
                    Some(GoFishAction::Reveal(vec![]).into())
                }
            }
            Running(PlayerId::This, Fishing(_)) => None,
            Running(PlayerId::This, Awaiting(_, _)) => None,
            Running(PlayerId::This, ReceivedResponse(_, rank, num_cards)) => {
                if num_cards > &0 {
                    let mut cards = self.hand.take_matching(*rank);
                    if cards.len() % 2 == 1 {
                        self.hand.extend(vec![cards.pop().unwrap()]);
                    }
                    Some(GoFishAction::Reveal(cards).into())
                } else if state.top_of_deck() == None {
                    Some(GoFishAction::Reveal(vec![]).into())
                } else {
                    Some(GoFishAction::Draw.into())
                }
            }
            Running(PlayerId::This, AfterFishing(rank)) => {
                let mut matching = self.hand.take_matching(*rank);
                if matching.len() % 2 != 0 {
                    self.hand.extend(matching.pop().into_iter().collect());
                }
                Some(GoFishAction::Reveal(matching).into())
            }
            Running(Other(o), Awaiting(PlayerId::This, rank)) => {
                let cards = (self.on_request)(&mut self.hand, rank);
                Some(GoFishAction::Transfer((*o, cards)).into())
            }
            Running(Other(o), Fishing(_)) => {
                Some(PlayerAction::RevealCard(*o, *state.top_of_deck().unwrap()))
            }
            Running(Other(_), _) => None,
        }
    }

    fn receive(&mut self, cards: Vec<AttestedGoFishCard>) {
        self.hand.extend(cards)
    }

    fn drew(&mut self, card: AttestedGoFishCard) {
        self.hand.extend(vec![card]);
        info!("hand len is now {:?}", self.hand.len());
    }

    fn hand(&self) -> &Hand<AttestedGoFishCard> {
        &self.hand
    }

    fn on_action_error(&mut self, _error: PlayerActionError) -> Box<dyn std::error::Error> {
        unreachable!("AutoPlayer does not make mistakes")
    }
}

impl AutoPlayer {
    pub(super) fn new() -> Self {
        Self {
            hand: Default::default(),
            on_request: Box::new(|hand: &mut Hand<AttestedGoFishCard>, rank: &Rank| {
                hand.take_matching(*rank)
            }),
        }
    }

    #[cfg(test)]
    pub(super) fn new_cheater(mut num_cheats: usize) -> Self {
        Self {
            hand: Default::default(),
            on_request: Box::new(move |hand: &mut Hand<AttestedGoFishCard>, rank: &Rank| {
                info!("have {} cheats left", num_cheats);
                let num_cards = hand.matching_rank(*rank).count();
                if num_cheats > 0 && num_cards > 0 {
                    num_cheats -= 1;
                    info!(
                        "cheating; have {} cards of rank {} but saying none",
                        num_cards, rank
                    );
                    return vec![];
                }
                hand.take_matching(*rank)
            }),
        }
    }
}
