use mental_poker::{
    game::message::{
        HiddenMessageToken, InputMessage, InputMessageToken, OutputMessage, OutputMessageToken,
        PrivateMessageToken, PublicMessageToken,
    },
    OtherPlayerId, PlayerId,
};
use thiserror::Error;

use crate::cards::*;

#[derive(Clone, Debug)]
pub(crate) enum TransferAction {
    ToSelf(Vec<AttestedGoFishCard>),
    ToOther(OtherPlayerId, usize),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum GoFishAction<T, ReceiverId> {
    Draw,
    Reveal(Vec<AttestedGoFishCard>),
    Request { to: ReceiverId, rank: Rank },
    Transfer(T),
}

#[derive(Debug, Clone, Error)]
pub(crate) enum GoFishParseError<T> {
    #[error("Found unexpected token {0:?}")]
    Unexpected(T, Vec<T>),
    #[error("Expecting more input tokens")]
    EndOfInput,
    #[error("Found extra input tokens: {0:?}, {1:?}")]
    ExtraInput(T, Vec<T>),
}

impl From<GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>>
    for OutputMessage<GoFishDeck>
{
    fn from(action: GoFishAction<(OtherPlayerId, Vec<AttestedGoFishCard>), OtherPlayerId>) -> Self {
        use OutputMessageToken::*;
        type Pub = PublicMessageToken<GoFishDeck>;
        type Priv = PrivateMessageToken<GoFishDeck>;
        match action {
            GoFishAction::Draw => [Public(Pub::Value(0))].into_iter().collect(),
            GoFishAction::Reveal(cards) => [Pub::Value(1)]
                .into_iter()
                .chain(cards.into_iter().map(|c| Pub::AttestedCard(c.into())))
                .map(Public)
                .collect(),
            GoFishAction::Request { to, rank } => [
                Pub::Value(2),
                Pub::Player(to.into()),
                Pub::Value(Into::<u8>::into(rank).into()),
            ]
            .into_iter()
            .map(Public)
            .collect(),
            GoFishAction::Transfer((recipient, cards)) => {
                [Public(Pub::Value(3)), Public(Pub::Player(recipient.into()))]
                    .into_iter()
                    .chain(
                        cards
                            .into_iter()
                            .map(|d| Private(recipient.into(), Priv::AttestedCard(d.into()))),
                    )
                    .collect()
            }
        }
    }
}

impl TryFrom<InputMessage<GoFishDeck>> for GoFishAction<TransferAction, PlayerId> {
    type Error = GoFishParseError<InputMessageToken<GoFishDeck>>;

    fn try_from(message: InputMessage<GoFishDeck>) -> Result<Self, Self::Error> {
        use GoFishAction::*;
        use InputMessageToken::*;

        type Pub = PublicMessageToken<GoFishDeck>;
        type Priv = PrivateMessageToken<GoFishDeck>;
        type Hid = HiddenMessageToken;

        use combine::{choice, many, satisfy_map, token, EasyParser, ParseError, Parser};

        let draw = token(Public(Pub::Value(0))).map(|_| Draw);
        let reveal = token(Public(Pub::Value(1))).with(
            many(satisfy_map(|t| match t {
                Public(Pub::AttestedCard(c)) => Some(AttestedGoFishCard::from(c)),
                _ => None,
            }))
            .map(Reveal),
        );
        let request = token(Public(Pub::Value(2))).with(
            satisfy_map(|t| match t {
                Public(Pub::Player(p)) => Some(p),
                _ => None,
            })
            .and(satisfy_map(|t| match t {
                Public(Pub::Value(v)) => v.try_into().ok().and_then(|r: u8| r.try_into().ok()),
                _ => None,
            }))
            .map(|(to, rank)| Request { to, rank }),
        );
        let response = token(Public(Pub::Value(3))).with(choice!(
            token(Public(Pub::Player(PlayerId::This))).with(
                many(satisfy_map(|t| match t {
                    Private(Priv::AttestedCard(c)) => Some(AttestedGoFishCard::from(c)),
                    _ => None,
                }))
                .map(|cards| Transfer(TransferAction::ToSelf(cards)))
            ),
            satisfy_map(|t| match t {
                Public(Pub::Player(PlayerId::Other(o))) => Some(o),
                _ => None,
            })
            .then(|o| many(token(Hidden(o, Hid::AttestedCard)).map(|_| ()))
                .map(move |cards: Vec<()>| Transfer(TransferAction::ToOther(o, cards.len()))),)
        ));

        let tokens = message.into_iter().collect::<Vec<_>>();
        match choice!(draw, reveal, request, response).easy_parse(&tokens[..]) {
            Ok((t, remaining)) => match remaining.split_first() {
                Some((front, rest)) => Err(GoFishParseError::ExtraInput(
                    *front,
                    rest.iter().cloned().collect(),
                )),
                None => Ok(t),
            },
            Err(e) => Err(if e.is_unexpected_end_of_input() {
                GoFishParseError::EndOfInput
            } else {
                let tokens = &tokens[e.position().translate_position(&tokens[..])..];
                let (front, rest) = tokens.split_first().unwrap();
                GoFishParseError::Unexpected(*front, rest.iter().cloned().collect())
            }),
        }
    }
}
