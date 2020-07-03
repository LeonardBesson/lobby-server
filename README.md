# Lobby Server

Central game lobby server written in Elixir

* Compact binary protocol based on [Bincode](https://github.com/LeonardBesson/bincode).
* Postgres for relational and persistent data.
* Mnesia for ephemeral data and caching.

## Planned Features

- [x] Client authentication
- [x] User bans
- [x] Lobbies
- [x] Friend lists
- [ ] Group chat
- [x] Private chat
- [ ] Server browsing
- [ ] Matchmaking
- [ ] Clustering

## Protocol

Networking is built on top of TCP since latency is not critical and ordering and reliability are more desirable.

A set of known messages are exchanged between clients and server. Messages are transformed into packets before transmission over the network according to the following structure:

```
|-----------------------------------------------|--------------|
| Packet Header                                 | Packet Data  |
|-----------------|--------------|--------------|--------------|
| Flags           | Message Type | Data Size    | Bincode Data |
| 8 bits          | 8 or 16 bits | 8 or 24 bits | ...          |
|-----------------|--------------|--------------|--------------|
```

Flags:
```
|-----|------------|------------------------------|
| Bit | Name       | Value                        |
|-----|------------|------------------------------|
|  7  | Fixed      | Always 1                     |
|-----|------------|------------------------------|
|  6  | Short type | 1 -> Message type is 8 bits  |
|     |            | 0 -> Message type is 16 bits |
|-----|------------|------------------------------|
|  5  | Short size | 1 -> Data size is 8 bits     |
|     |            | 0 -> Data size is 24 bits    |
|-----|------------|------------------------------|
| ... |    ...     | Reserved for future use      |
|-----|------------|------------------------------|
```

Message type and size are big endian.
Packet data is simply the Bincode encoded message.
