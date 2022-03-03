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
        use GoFishParseError::*;
        use InputMessageToken::*;

        type Pub = PublicMessageToken<GoFishDeck>;
        type Priv = PrivateMessageToken<GoFishDeck>;
        type Hid = HiddenMessageToken;

        let mut tokens = message.into_iter();

        match tokens.next() {
            Some(Public(Pub::Value(0))) => match tokens.next() {
                None => Ok(Draw),
                Some(s) => Err(ExtraInput(s, tokens.collect())),
            },
            Some(Public(Pub::Value(1))) => tokens
                .try_fold(vec![], |mut v, item| match item {
                    Public(Pub::AttestedCard(card)) => {
                        v.push(card.into());
                        Ok(v)
                    }
                    x => Err(x),
                })
                .map(Reveal)
                .map_err(|e| Unexpected(e, tokens.collect())),
            Some(Public(Pub::Value(2))) => match tokens.next() {
                Some(Public(Pub::Player(to))) => match tokens.next() {
                    Some(r) => match r {
                        Public(Pub::Value(rank)) => match tokens.next() {
                            Some(s) => Err(ExtraInput(s, tokens.collect())),
                            None => Ok(Request {
                                to,
                                rank: rank
                                    .try_into()
                                    .map_err(|_| ())
                                    .and_then(|rank: u8| Rank::try_from(rank).map_err(|_| ()))
                                    .map_err(|_| Unexpected(r, tokens.collect()))?,
                            }),
                        },
                        s => Err(Unexpected(s, tokens.collect())),
                    },
                    None => Err(EndOfInput),
                },
                Some(s) => Err(Unexpected(s, tokens.collect())),
                None => Err(EndOfInput),
            },
            Some(Public(Pub::Value(3))) => match tokens.next() {
                Some(Public(Pub::Player(PlayerId::This))) => tokens
                    .try_fold(vec![], |mut v, item| match item {
                        Private(Priv::AttestedCard(card)) => {
                            v.push(card.into());
                            Ok(v)
                        }
                        x => Err(x),
                    })
                    .map(|cards| Transfer(TransferAction::ToSelf(cards)))
                    .map_err(|e| Unexpected(e, tokens.collect())),
                Some(Public(Pub::Player(PlayerId::Other(p)))) => tokens
                    .try_fold(0, |count, item| match item {
                        Hidden(o, Hid::AttestedCard) if o == p => Ok(count + 1),
                        x => Err(x),
                    })
                    .map(|count| Transfer(TransferAction::ToOther(p, count)))
                    .map_err(|e| Unexpected(e, tokens.collect())),
                Some(s) => Err(Unexpected(s, tokens.collect())),
                None => Err(EndOfInput),
            },
            Some(x) => Err(Unexpected(x, tokens.collect())),
            None => Err(EndOfInput),
        }
    }
}
