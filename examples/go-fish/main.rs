#![allow(incomplete_features)]
#![feature(generic_const_exprs)]
#![feature(generic_associated_types)]
#![feature(associated_type_bounds)]
#![feature(type_alias_impl_trait)]
#![feature(bool_to_option)]
#![feature(step_trait)]

use log::{info, warn};
use mental_poker::game::trusting::{Bootstrap, Game};
use mental_poker::game::{CardGame, TurnOrderProvider as _};
use mental_poker::PlayerId;
use structopt::StructOpt;

use cards::*;
use game::GoFish;
use message::*;
use player::*;

pub(crate) mod cards;
pub(crate) mod game;
pub(crate) mod message;
pub(crate) mod player;

#[derive(StructOpt)]
struct Args {
    computer_players: u8,
    repl_players: u8,
}

fn bootstrap_games<I: Iterator<Item = Bootstrap>>(
    players: I,
) -> Vec<(Vec<PlayerId>, Game<GoFish>)> {
    let mut players = players.collect::<Vec<_>>();
    for i in 0..players.len() {
        for j in 0..players.len() {
            if i == j {
                continue;
            }
            let id = players[j].id();
            players[i].add_player(id).unwrap();
        }
    }
    players
        .into_iter()
        .map(|p| ((&p).turn_order().collect(), p.start()))
        .collect::<Vec<_>>()
}

fn new_bootstrap() -> mental_poker::game::trusting::Bootstrap {
    mental_poker::game::trusting::Bootstrap::new()
}

fn run_players<G: CardGame<Game = GoFish> + std::fmt::Debug>(players: &mut Vec<Player<G>>)
where
    <G as CardGame>::InputError: 'static,
{
    let mut total_iterations = 0;
    while players.iter().any(|p| p.playing()) {
        let messages = players
            .iter_mut()
            .enumerate()
            .filter_map(|(i, p)| match p.actions() {
                Ok(Some(m)) => Some((i, m)),
                Ok(None) => None,
                Err(e) => {
                    warn!("error: {}", e);
                    None
                }
            })
            .collect::<Vec<_>>();

        total_iterations += 1;

        for (i, p) in players.iter().enumerate() {
            info!(
                "player {}: state: {:?}; hand: {:?}",
                i,
                p.state().map(|s| s.state()),
                p.hand()
            );
        }
        if total_iterations >= 1_000 {
            panic!("can't keep playing");
        }

        for (sender, message) in messages {
            for (receiver, player) in players.iter_mut().enumerate() {
                if sender == receiver {
                    continue;
                }
                match player.receive(message.clone()) {
                    Ok(_) => (),
                    Err(e) => warn!("rejected: {}", e),
                }
            }
        }
    }
}

fn main() -> () {
    let args = Args::from_args();
    env_logger::init();
    let players_start = (0u8..(args.computer_players + args.repl_players)).map(|_| new_bootstrap());

    let players = bootstrap_games(players_start);

    let mut players = players
        .into_iter()
        .enumerate()
        .map(|(i, (turn_order, game))| {
            let i: u8 = i.try_into().unwrap();
            let player_type = if i < args.repl_players {
                PlayerType::Cli
            } else {
                PlayerType::Auto
            };
            Player::new(i, game, turn_order, player_type)
        })
        .collect();

    run_players(&mut players);

    players.into_iter().for_each(|p| p.validate().unwrap());
}

#[cfg(test)]
mod tests {
    use crate::game::{ActionError, TransferError};

    use super::*;
    use assert_matches::assert_matches;
    use test_log::test;

    #[test]
    fn two_players() {
        let players = bootstrap_games((0..2).map(|_| new_bootstrap()));

        let mut players = players
            .into_iter()
            .enumerate()
            .map(|(i, (turn_order, game))| {
                Player::new(i.try_into().unwrap(), game, turn_order, PlayerType::Auto)
            })
            .collect();

        run_players(&mut players);
    }

    #[test]
    fn three_players() {
        let players = bootstrap_games((0..3).map(|_| new_bootstrap()));

        let mut players = players
            .into_iter()
            .enumerate()
            .map(|(i, (turn_order, game))| {
                Player::new(i.try_into().unwrap(), game, turn_order, PlayerType::Auto)
            })
            .collect();

        run_players(&mut players);

        players.into_iter().for_each(|p| p.validate().unwrap());
    }

    #[test]
    fn two_players_one_cheater() {
        let players = bootstrap_games((0..3).map(|_| new_bootstrap()));

        let mut players = players
            .into_iter()
            .enumerate()
            .map(|(i, (turn_order, game))| {
                Player::new(
                    i.try_into().unwrap(),
                    game,
                    turn_order,
                    if i == 0 {
                        PlayerType::AutoCheating(10)
                    } else {
                        PlayerType::Auto
                    },
                )
            })
            .collect();

        run_players(&mut players);

        let result = players.into_iter().enumerate().try_for_each(|(i, p)| {
            println!("validating player {}", i);
            p.validate()
        });
        match result {
            Ok(()) => (),
            Err(e) => {
                assert_matches!(
                    e,
                    ValidationError::TransferError(ActionError::Action(
                        TransferError::SomeCardsUntransferred
                    ))
                );
                log::info!("Found the cheater");
            }
        }
    }

    #[test]
    fn three_players_one_cheater() {
        let players = bootstrap_games((0..3).map(|_| new_bootstrap()));

        let mut players = players
            .into_iter()
            .enumerate()
            .map(|(i, (turn_order, game))| {
                Player::new(
                    i.try_into().unwrap(),
                    game, turn_order,
                    if i == 0 {
                        PlayerType::AutoCheating(10)
                    } else {
                        PlayerType::Auto
                    },
                )
            })
            .collect();

        run_players(&mut players);

        let result = players.into_iter().enumerate().try_for_each(|(i, p)| {
            println!("validating player {}", i);
            p.validate()
        });
        match result {
            Ok(()) => (),
            Err(e) => {
                assert_matches!(
                    e,
                    ValidationError::TransferError(ActionError::Action(
                        TransferError::SomeCardsUntransferred
                    ))
                );
                log::info!("Found the cheater");
            }
        }
    }
}
