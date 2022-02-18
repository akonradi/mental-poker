use std::io::Write;

use log::warn;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{digit1, one_of, space1},
    combinator::{map, map_res},
    multi::{many1, separated_list0},
    sequence::{separated_pair, tuple},
    Finish, IResult,
};

use crate::{
    cards::GoFishDeck,
    game::{DealState, GameState, Hand, TurnState},
    player::{OtherPlayerHands, PlayerHand},
};
use mental_poker::{
    game::AttestedCard,
    game::CardGame,
    PlayerId::{self, *},
};

use crate::{
    cards::UnknownCard,
    cards::{AttestedGoFishCard, Rank},
    game::GoFishState,
    message::GoFishAction,
};

use super::{PlayerAction, PlayerImpl};

pub(crate) struct CliPlayer {
    index: u8,
    hand: Hand<AttestedGoFishCard>,
}

enum Action {
    Draw,
    Reveal(Vec<u8>),
    Ask(u8, Rank),
    Send(Option<u8>, Vec<u8>),
    Allow,
}

impl CliPlayer {
    fn parse_action(s: &str) -> Result<(&str, Action), nom::error::Error<&str>> {
        fn parse_u8(s: &str) -> IResult<&str, u8> {
            map_res(digit1, |s: &str| s.parse())(s)
        }
        fn cards_list(s: &str) -> IResult<&str, Vec<u8>> {
            separated_list0(many1(one_of(", ")), parse_u8)(s)
        }
        fn rank(s: &str) -> IResult<&str, Rank> {
            map_res(parse_u8, |r| Rank::try_from(r))(s)
        }

        let draw = map(tag("draw"), |_| Action::Draw);
        let reveal = alt((
            map(
                separated_pair(tag("reveal"), space1, cards_list),
                |(_, b)| Action::Reveal(b),
            ),
            map(tag("reveal"), |_| Action::Reveal(vec![])),
        ));
        let ask = map(
            tuple((
                tag("ask"),
                space1,
                parse_u8,
                space1,
                tag("for"),
                space1,
                rank,
            )),
            |(_, _, a, _, _, _, b)| Action::Ask(a, b),
        );
        let send = map(
            tuple((
                tag("send"),
                space1,
                alt((map(tag("none"), |_| vec![]), cards_list)),
                nom::combinator::opt(map(
                    tuple((space1, tag("to"), space1, parse_u8)),
                    |(_, _, _, u)| u,
                )),
            )),
            |(_, _, a, b)| Action::Send(b, a),
        );
        let allow = map(tag("allow"), |_| Action::Allow);
        alt((draw, reveal, ask, send, allow))(s).finish()
    }

    fn parse_one_line(
        &mut self,
        state: &mut GoFishState<UnknownCard>,
        input: &str,
    ) -> Result<PlayerAction, Box<dyn std::error::Error>> {
        let parsed = match Self::parse_action(input) {
            Ok(p) => p.1,
            Err(e) => return Err(format!("failed to parse {}", e).into()),
        };
        match parsed {
            Action::Draw => return Ok(GoFishAction::Draw.into()),
            Action::Allow => {
                let other = match state.state() {
                    GameState::Dealing(Other(o), _) => o,
                    GameState::Running(Other(o), TurnState::Fishing(_)) => o,
                    _ => return Err(format!("no other player drawing").into()),
                };
                let top = match state.top_of_deck() {
                    Some(top) => top,
                    None => return Err(format!("no more cards").into()),
                };
                return Ok(PlayerAction::RevealCard(*other, *top));
            }
            Action::Reveal(cards) => {
                let cards: Result<Vec<_>, _> = cards
                    .into_iter()
                    .map(|i| {
                        self.hand
                            .iter()
                            .find(|c| Into::<AttestedCard<_>>::into(**c).card().index() == i.into())
                            .cloned()
                            .ok_or_else(|| format!("unknown card {}", i))
                    })
                    .collect();
                return Ok(GoFishAction::Reveal(self.hand.remove_cards(&cards?).unwrap()).into());
            }
            Action::Ask(player_id, rank) => {
                let player = state
                    .players()
                    .find_map(|(i, _)| match i {
                        Other(o) if u8::from(o) == player_id => Some(o),
                        _ => None,
                    })
                    .ok_or(format!("not a valid player: {:?}", player_id))?;
                return Ok(GoFishAction::Request { to: *player, rank }.into());
            }
            Action::Send(player_id, cards) => {
                let cards: Result<Vec<_>, _> = cards
                    .into_iter()
                    .map(|i| {
                        self.hand
                            .iter()
                            .find(|c| Into::<AttestedCard<_>>::into(**c).card().index() == i.into())
                            .cloned()
                            .ok_or_else(|| format!("unknown card {}", i))
                    })
                    .collect();

                let player = match player_id {
                    Some(player_id) => state
                        .players()
                        .find_map(|(i, _)| match i {
                            Other(o) if u8::from(o)  == player_id => Some(o),
                            _ => None,
                        })
                        .ok_or(format!("not a valid player: {:?}", player_id))?,
                    None => match state.state() {
                        GameState::Running(Other(o), TurnState::Awaiting(PlayerId::This, _)) => o,
                        _ => return Err(format!("couldn't infer player").into()),
                    },
                };

                return Ok(GoFishAction::Transfer((
                    *player,
                    self.hand.remove_cards(&cards?).unwrap(),
                ))
                .into());
            }
        }
    }
}

impl<G: CardGame> PlayerImpl<G> for CliPlayer {
    fn action(&mut self, state: &mut GoFishState<UnknownCard>) -> Option<PlayerAction> {
        use DealState::*;
        use GameState::*;
        use TurnState::*;

        match state.state() {
            Dealing(PlayerId::This, Starting) if self.hand.len() < state.deal_size() => {
                return Some(GoFishAction::Draw.into())
            }
            Dealing(PlayerId::This, Starting) => (),
            Dealing(Other(o), Drawing) => match state.top_of_deck() {
                Some(top) => return Some(PlayerAction::RevealCard(*o, *top)),
                None => (),
            },
            Running(Other(o), Fishing(_)) => match state.top_of_deck() {
                Some(top) => return Some(PlayerAction::RevealCard(*o, *top)),
                None => (),
            },
            Running(PlayerId::This, Playing) => (),
            Running(PlayerId::This, ReceivedResponse(_, _, _)) => (),
            Running(PlayerId::This, AfterFishing(_)) => (),
            Running(Other(_), Awaiting(PlayerId::This, _)) => (),
            _ => return None,
        };
        loop {
            let other_players = OtherPlayerHands(
                state
                    .players()
                    .filter_map(|(id, p)| match id {
                        PlayerId::This => None,
                        Other(o) => Some((o, p.hand())),
                    })
                    .collect(),
            );
            println!(
                "# Your (local player {}'s) cards: ({}) {}",
                self.index,
                self.hand.len(),
                PlayerHand(&self.hand)
            );
            println!("#   other player hands: {}", other_players);
            println!("#   state: {}", state.state());
            println!(
                "#   deck: {} cards left",
                state
                    .top_of_deck()
                    .map_or(0, |p| <GoFishDeck as mental_poker::DeckType>::SIZE
                        - 1
                        - p.index())
            );
            print!("> ");
            std::io::stdout().flush().unwrap();

            let mut input = String::new();
            std::io::stdin().read_line(&mut input).unwrap();
            match self.parse_one_line(state, &input) {
                Ok(a) => return Some(a),
                Err(e) => {
                    warn!("failed to parse: {}", e);
                    continue;
                }
            }
        }
    }

    fn receive(&mut self, cards: Vec<AttestedGoFishCard>) {
        self.hand.extend(cards)
    }

    fn drew(&mut self, card: AttestedGoFishCard) {
        self.hand.extend(vec![card])
    }

    fn hand(&self) -> &Hand<AttestedGoFishCard> {
        &self.hand
    }
}

impl CliPlayer {
    pub(super) fn new(i: u8) -> Self {
        Self {
            hand: Default::default(),
            index: i,
        }
    }
}
