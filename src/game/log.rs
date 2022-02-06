use crate::{
    game::{message::PublicEvent, AttestedCard, CardGame, GameInput, GameType},
    DeckPosition, DeckType, OtherPlayerId, PlayerId,
};

/// A game whose messages can be saved and decoded later for analysis.
pub trait LoggableGameType: GameType {
    /// The type of all game events.
    ///
    /// This type must be constructible from the `PlayerId` of the sender and
    /// the event.
    type GameEvent: TryFrom<(PlayerId, PublicEvent<Self::Deck>)> + Clone;
}

/// Describes a completed card game, where the de-obfuscated events of the game
/// can be examined.
pub trait FinishedCardGame<G: GameType> {
    type LogItem;
    type LogIter<'a>: Iterator<Item = Self::LogItem>
    where
        Self: 'a;

    /// Returns an iterator that yields the list of game messages.
    fn log(&self) -> Self::LogIter<'_>;

    /// Returns the set of cards in in the shuffled deck.
    fn cards(&self) -> &[AttestedCard<G::Deck>; G::Deck::SIZE];
}

/// A type that can be used to decode saved messages of the provided card game
/// into [`LoggableGameType::GameEvent`]s.
pub trait GameMessageDecoder<C: CardGame>
where
    C::Game: LoggableGameType,
{
    type DecodeError: std::error::Error;
    fn decode_message(
        &self,
        message: &C::GameMessage,
    ) -> Result<<C::Game as LoggableGameType>::GameEvent, Self::DecodeError>;
}

/// Wraps a [`CardGame`] implementation and logs all input and output game
/// messages.
pub struct LoggingGame<C: CardGame>
where
    C::Game: LoggableGameType,
{
    game: C,
    log: Vec<C::GameMessage>,
}

impl<C: CardGame> LoggingGame<C>
where
    C::Game: LoggableGameType,
{
    pub fn new(game: C) -> Self {
        Self { game, log: vec![] }
    }
}

impl<C: CardGame> CardGame for LoggingGame<C>
where
    C::Game: LoggableGameType,
{
    type Game = C::Game;
    type UniqueId = C::UniqueId;
    type PlayerIter = C::PlayerIter;
    type InputError = C::InputError;
    type OutputError = C::OutputError;
    type GameMessage = C::GameMessage;

    fn other_players(&self) -> Self::PlayerIter {
        let Self { game, log: _ } = self;
        game.other_players()
    }

    fn send(
        &mut self,
        action: <C::Game as GameType>::Action,
    ) -> Result<Self::GameMessage, Self::OutputError> {
        let Self { game, log } = self;
        let message = game.send(action)?;
        log.push(message.clone());
        Ok(message)
    }

    fn receive(
        &mut self,
        message: Self::GameMessage,
    ) -> Result<Option<GameInput<C::Game>>, Self::InputError> {
        let Self { game, log } = self;
        let input = game.receive(message.clone())?;
        log.push(message);
        Ok(input)
    }

    fn reveal(
        &mut self,
        to: OtherPlayerId,
        pos: DeckPosition<<C::Game as GameType>::Deck>,
    ) -> Self::GameMessage {
        let Self { game, log } = self;
        let message = game.reveal(to, pos);
        log.push(message.clone());
        message
    }

    type FinishInput = C::FinishInput;
    type FinishedGame = FinishedLoggedGame<C>;
    type FinishError = C::FinishError;

    fn finish(self, input: Self::FinishInput) -> Result<Self::FinishedGame, Self::FinishError> {
        let Self { game, log } = self;
        let game = game.finish(input)?;
        Ok(FinishedLoggedGame { game, log })
    }
}

/// Wraps a finished card game and log of messages.
pub struct FinishedLoggedGame<C: CardGame>
where
    C::Game: LoggableGameType,
{
    game: C::FinishedGame,
    log: Vec<C::GameMessage>,
}

impl<C: CardGame> FinishedLoggedGame<C>
where
    C::Game: LoggableGameType,
{
    pub fn game(&self) -> &C::FinishedGame {
        &self.game
    }
}

pub trait DeckProvider<D: DeckType> {
    fn cards(&self) -> &[AttestedCard<D>; D::SIZE];
}

impl<C: CardGame> FinishedCardGame<C::Game> for FinishedLoggedGame<C>
where
    C::Game: LoggableGameType,
    C::FinishedGame: GameMessageDecoder<C> + DeckProvider<<C::Game as GameType>::Deck>,
{
    type LogItem = Result<
        <C::Game as LoggableGameType>::GameEvent,
        <C::FinishedGame as GameMessageDecoder<C>>::DecodeError,
    >;
    type LogIter<'a>
    where
        C: 'a,
    = FinishedLogIter<'a, C>;

    fn log(&self) -> Self::LogIter<'_> {
        let it = self.log.iter();
        FinishedLogIter {
            it,
            game: &self.game,
        }
    }

    fn cards(
        &self,
    ) -> &[AttestedCard<<C::Game as GameType>::Deck>; <C::Game as GameType>::Deck::SIZE] {
        self.game.cards()
    }
}

pub struct FinishedLogIter<'a, C: CardGame> {
    it: std::slice::Iter<'a, C::GameMessage>,
    game: &'a C::FinishedGame,
}

impl<'a, C: CardGame> Iterator for FinishedLogIter<'a, C>
where
    C::Game: LoggableGameType,
    C::FinishedGame: GameMessageDecoder<C>,
{
    type Item = Result<
        <C::Game as LoggableGameType>::GameEvent,
        <C::FinishedGame as GameMessageDecoder<C>>::DecodeError,
    >;

    fn next(&mut self) -> Option<Self::Item> {
        self.it
            .next()
            .map(|message| self.game.decode_message(message))
    }
}

#[cfg(test)]
mod tests {
    use either::Either;
    use thiserror::Error;

    use super::*;
    use crate::deck::testutil::FakeDeck;
    use crate::game::testutil::FakeGameType;
    use crate::game::{OutputMessage, PublicMessageToken, RevealedMessageToken};

    #[derive(Debug, Error, PartialEq, Eq)]
    pub enum FakeOutputError {}
    #[derive(Debug, Error, PartialEq, Eq)]
    pub enum FakeInputError {}
    #[derive(Debug, Error, PartialEq, Eq)]
    pub enum FakeFinishError {}
    #[derive(Debug, Error, PartialEq, Eq)]
    pub enum FakeDecodeError {}

    pub struct FakeFinishedGame;

    mockall::mock! {
        pub CardGame {}
        impl CardGame for CardGame {
            type Game = FakeGameType;
            type UniqueId = u8;
            type OutputError = FakeOutputError;
            type InputError = FakeInputError;
            type GameMessage = (PlayerId, Either<u64, (DeckPosition<FakeDeck>, PlayerId)>);
            type PlayerIter = std::vec::IntoIter<OtherPlayerId>;
            type FinishInput = ();
            type FinishedGame = FakeFinishedGame;
            type FinishError = FakeFinishError;

            fn other_players(&self) -> std::vec::IntoIter<OtherPlayerId>;

            fn send(
                &mut self,
                action: <FakeGameType as GameType>::Action,
            ) -> Result<(PlayerId, Either<u64, (DeckPosition<FakeDeck>, PlayerId)>), FakeOutputError>;

            fn receive(
                &mut self,
                message: (PlayerId, Either<u64, (DeckPosition<FakeDeck>, PlayerId)> )
            ) -> Result<Option<GameInput<FakeGameType>>, FakeInputError>;

            fn reveal(
                &mut self,
                to: OtherPlayerId,
                pos: DeckPosition<FakeDeck>,
            ) -> (PlayerId, Either<u64, (DeckPosition<FakeDeck>, PlayerId)>);

            fn finish(self, input: ()) -> Result<FakeFinishedGame, FakeFinishError>;
        }
    }

    impl FinishedCardGame<FakeGameType> for () {
        type LogItem = ();

        type LogIter<'a> = std::vec::IntoIter<Self::LogItem>;

        fn log(&self) -> Self::LogIter<'_> {
            vec![].into_iter()
        }

        fn cards(&self) -> &[AttestedCard<FakeDeck>; FakeDeck::SIZE] {
            todo!()
        }
    }

    impl GameMessageDecoder<MockCardGame> for FakeFinishedGame {
        type DecodeError = FakeDecodeError;

        fn decode_message(
            &self,
            message: &<MockCardGame as CardGame>::GameMessage,
        ) -> Result<<FakeGameType as LoggableGameType>::GameEvent, Self::DecodeError> {
            let (player, message) = message;
            Ok((
                player.clone(),
                match message {
                    Either::Left(value) => PublicEvent::Message(
                        [RevealedMessageToken::Public(PublicMessageToken::Value(
                            *value,
                        ))]
                        .into_iter()
                        .collect(),
                    ),
                    Either::Right((pos, to)) => PublicEvent::Reveal(*pos, *to),
                },
            ))
        }
    }

    impl DeckProvider<FakeDeck> for FakeFinishedGame {
        fn cards(&self) -> &[AttestedCard<FakeDeck>; FakeDeck::SIZE] {
            todo!()
        }
    }

    #[test]
    fn logging_game() {
        impl LoggableGameType for FakeGameType {
            type GameEvent = (PlayerId, PublicEvent<FakeDeck>);
        }

        let mut mock_game = MockCardGame::default();
        mock_game
            .expect_send()
            .return_once(|_| Ok((PlayerId::This, Either::Left(17495))));
        mock_game.expect_receive().return_once(|_| {
            Ok(Some(GameInput::Message(
                OtherPlayerId::new(4),
                FromIterator::from_iter([]),
            )))
        });
        mock_game.expect_reveal().return_once(|_, _| {
            (
                PlayerId::This,
                Either::Right((DeckPosition::new(3), OtherPlayerId(1).into())),
            )
        });
        mock_game
            .expect_finish()
            .return_once(|_| Ok(FakeFinishedGame));

        let mut game = LoggingGame::new(mock_game);

        let send_result = game.send(OutputMessage::from_iter([]));
        let receive_result = game.receive((OtherPlayerId(2).into(), Either::Left(32)));
        let reveal_result = game.reveal(OtherPlayerId(1), DeckPosition::new(3));

        assert_eq!(send_result, Ok((PlayerId::This, Either::Left(17495))));
        assert_eq!(
            receive_result,
            Ok(Some(
                GameInput::Message(OtherPlayerId::new(4), FromIterator::from_iter([]),)
            ))
        );
        assert_eq!(
            reveal_result,
            (
                PlayerId::This,
                Either::Right((DeckPosition::new(3), OtherPlayerId(1).into()))
            )
        );

        let finish = game.finish(()).unwrap();
        let log: Result<_, _> = finish.log().collect();
        let log: Vec<_> = log.unwrap();
        assert_eq!(
            log,
            vec![
                (
                    PlayerId::This,
                    PublicEvent::Message(
                        [RevealedMessageToken::Public(PublicMessageToken::Value(
                            17495
                        ))]
                        .into_iter()
                        .collect()
                    )
                ),
                (
                    OtherPlayerId(2).into(),
                    PublicEvent::Message(FromIterator::from_iter([RevealedMessageToken::Public(
                        PublicMessageToken::Value(32)
                    )]))
                ),
                (
                    PlayerId::This,
                    PublicEvent::Reveal(DeckPosition::new(3), OtherPlayerId(1).into())
                )
            ]
        );
    }
}
