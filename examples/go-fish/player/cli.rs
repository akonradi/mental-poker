use std::{io::Write, str::FromStr};

use combine::{
    choice, easy, from_str, optional,
    parser::{char::string, combinator::lazy, range::take_while1, repeat::sep_by1},
    skip_many1, EasyParser, Parser,
};
use log::warn;

use mental_poker::{
    game::AttestedCard,
    game::CardGame,
    PlayerId::{self, *},
};

use crate::{
    cards::{AttestedGoFishCard, GoFishDeck, Rank, UnknownCard},
    game::{DealState, GameState, GoFishState, Hand, TurnState},
    message::GoFishAction,
    player::{OtherPlayers, PlayerAction, PlayerActionError, PlayerHand, PlayerImpl},
};

pub(crate) struct CliPlayer {
    index: u8,
    hand: Hand<AttestedGoFishCard>,
}

struct RankFromStr(Rank);

#[derive(thiserror::Error, Debug)]
enum RankFromStrErr {
    #[error("Invalid u8: {0}")]
    Parse(<u8 as FromStr>::Err),
    #[error("Not a valid rank: {0}")]
    Bound(<Rank as TryFrom<u8>>::Error),
}

impl FromStr for RankFromStr {
    type Err = RankFromStrErr;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        u8::from_str(s)
            .map_err(RankFromStrErr::Parse)
            .and_then(|u| {
                Rank::try_from(u)
                    .map(RankFromStr)
                    .map_err(RankFromStrErr::Bound)
            })
    }
}

impl From<RankFromStr> for Rank {
    fn from(RankFromStr(r): RankFromStr) -> Self {
        r
    }
}

#[cfg_attr(test, derive(Debug, Eq, PartialEq))]
enum Action {
    Draw,
    Reveal(Vec<u8>),
    Ask(u8, Rank),
    Send(Option<u8>, Vec<u8>),
    Allow,
}

impl CliPlayer {
    fn parse_action(s: &str) -> Result<(Action, &str), easy::ParseError<&str>> {
        let parse_u8 = || from_str::<_, u8, _>(take_while1(|c: char| c.is_digit(10)));
        let parse_rank =
            || from_str::<_, RankFromStr, _>(take_while1(|c: char| c.is_digit(10))).map(Into::into);
        let u8_list = || sep_by1(parse_u8(), choice!(string(", "), string(",")));
        let some_space = || skip_many1(string(" "));
        let draw = string("draw").map(|_| Action::Draw);
        let reveal = string("reveal").then(|_| {
            optional(some_space().with(lazy(u8_list)))
                .map(|cards| Action::Reveal(cards.unwrap_or(vec![])))
        });
        let ask = string("ask")
            .skip(some_space())
            .and(parse_u8())
            .skip(some_space())
            .skip(string("for"))
            .skip(some_space())
            .and(parse_rank())
            .map(|((_, to), r)| Action::Ask(to, r));
        let send = string("send")
            .skip(some_space())
            .and(choice!(string("none").map(|_| vec![]), u8_list()))
            .and(optional(
                some_space()
                    .skip(string("to"))
                    .skip(some_space())
                    .with(parse_u8()),
            ))
            .map(|((_, cards), player)| Action::Send(player, cards));
        let allow = string("allow").map(|_| Action::Allow);
        choice!(draw, reveal, ask, allow, send).easy_parse(s)
    }

    fn parse_one_line(
        &mut self,
        state: &mut GoFishState<UnknownCard>,
        input: &str,
    ) -> Result<PlayerAction, Box<dyn std::error::Error>> {
        let parsed = match Self::parse_action(input) {
            Ok((action, _)) => action,
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
                            Other(o) if u8::from(o) == player_id => Some(o),
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
        if state.game_over() {
            return None;
        }
        loop {
            let other_players = OtherPlayers(
                state
                    .players()
                    .filter_map(|(id, p)| match id {
                        PlayerId::This => None,
                        Other(o) => Some((o, p)),
                    })
                    .collect(),
            );
            println!(
                "# Your (local player {}'s) cards: ({}) {}",
                self.index,
                self.hand.len(),
                PlayerHand(&self.hand)
            );
            let this_player = state
                .players()
                .find_map(|(id, p)| if *id == PlayerId::This { Some(p) } else { None })
                .unwrap();
            println!(
                "# Your (local player {}'s) revealed count: {}",
                self.index,
                this_player.revealed().len(),
            );
            println!("#   other players: {}", other_players);
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

    fn on_action_error(&mut self, error: PlayerActionError) -> Box<dyn std::error::Error> {
        match error {
            PlayerActionError::RejectedReveal(cards, e) => {
                self.hand.extend(cards);
                e.into()
            }
            PlayerActionError::Other(e) => e,
        }
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

#[cfg(test)]
mod tests {
    use test_case::test_case;

    use super::*;

    const RANK_5: Rank = Rank::test_new(5);

    #[test_case("send 5" => Action::Send(None, vec![5]))]
    #[test_case("send 5 to 4" => Action::Send(Some(4), vec![5]))]
    #[test_case("send 5, 3" => Action::Send(None, vec![5, 3]))]
    #[test_case("send 2, 3, 4 to 9" => Action::Send(Some(9), vec![2, 3, 4]))]
    #[test_case("ask 3 for 5s" => Action::Ask(3, RANK_5))]
    #[test_case("draw" => Action::Draw)]
    #[test_case("reveal 3, 4" => Action::Reveal(vec![3, 4]))]
    #[test_case("reveal 2" => Action::Reveal(vec![2]))]
    #[test_case("ask 4 for 5s" => Action::Ask(4, RANK_5))]
    fn parse_valid_action(line: &str) -> Action {
        let (action, _) = CliPlayer::parse_action(line).unwrap();
        action
    }
}
