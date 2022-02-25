# mental-poker

This is a library for creating card game implementations without relying on a
trusted third party to shuffle or to enforce rules. The goal is to provide
cryptographic guarantees for clients who want to ensure fairness through an
easy-to-use set of primitives.

## Core operations

At its core, this library provides two operations to clients: the ability to
reveal the value of an unknown card, and the ability to publish messages. All
operations are completely public, though some parts are only decodable by a
subset of other players.

### Revealing cards

At the start of a card game, all cards are hidden and shuffled. The shuffling is
performed cooperatively by each of the game clients. The core abstraction that
this library provides to game client code is the ability to selectively reveal a
specific card from this shuffled deck to a specific player, without revealing it
to any other players. A given player can only learn about a given card in the
shuffled deck if all other players allow it.

### Sending messages.

Game clients notify each other of actions taken through messages. Messages
consist of public portions and private portions. The presence of a message and
the contents of each public portion are visible to all clients. The contents of
private portions are visible to a single player. When a messaging player creates
a message, it signals which portions of the message are public, and for each
private portion, which player it will be visible to.