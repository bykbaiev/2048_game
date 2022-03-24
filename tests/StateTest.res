open Test

test("#State.decodeBestScore: should decode best score got from local storage", () => {
  assertion(
    ~message = "Missing value",
    ~operator = "decodeBestScore",
    (a, b) => a == b,
    State.decodeBestScore(None),
    None
  )

  assertion(
    ~message = "Invalid value",
    ~operator = "decodeBestScore",
    (a, b) => a == b,
    State.decodeBestScore(Some("asdf")),
    None
  )
})

test("#State.decodeGameState: should decode from local storage's JSON string to actual state", () => {
  assertion(
    ~message = "Missing state",
    ~operator = "decodeGameState",
    (a, b) => a == b,
    State.decodeGameState(None),
    None
  )

  assertion(
    ~message = "Invalid state",
    ~operator = "decodeGameState",
    (a, b) => a == b,
    State.decodeGameState(Some("")),
    None
  )

  assertion(
    ~message = "Invalid state (structure)",
    ~operator = "decodeGameState",
    (a, b) => a == b,
    State.decodeGameState(Some("{\"score\": \"asdf\", \"tiles\": []}")),
    None
  )

  assertion(
    ~message = "Invalid state (structure 2)",
    ~operator = "decodeGameState",
    (a, b) => a == b,
    State.decodeGameState(Some("{\"status\": \"non-existing\", \"score\": 123, \"tiles\": []}")),
    None
  )

  assertion(
    ~message = "~ Invalid state (tiles are skipped)",
    ~operator = "decodeGameState",
    (a, b) => a == b,
    State.decodeGameState(Some("{\"status\": \"playing\", \"score\": 123, \"tiles\": [1, 2, 3]}")),
    Some(State.Playing({
      score: 123,
      best: None,
      tiles: list{}
    }))
  )

  assertion(
    ~message = "Empty tiles (playing)",
    ~operator = "decodeGameState",
    (a, b) => a == b,
    State.decodeGameState(Some("{\"status\": \"playing\", \"score\": 123, \"tiles\": []}")),
    Some(
      State.Playing({
        best: None,
        score: 123,
        tiles: list{}
      })
    )
  )

  assertion(
    ~message = "Valid state",
    ~operator = "decodeGameState",
    (a, b) => a == b,
    State.decodeGameState(Some("{\"status\": \"win\", \"score\": 123, \"tiles\": [{\"status\": \"average\", \"id\": \"tile-123\", \"val\": 4, \"x\": 0, \"y\": 1 }]}")),
    Some(
      State.Win({
        best: None,
        score: 123,
        tiles: list{
          Tile.GameTile.createTile(
            ~id  = "tile-123",
            ~val = 4,
            ~x   = 0,
            ~y   = 1
          ) -> Tile.GameTile.Converters.toAverage}
      })
    )
  )
})

test("#State.encodeHistoricalGameState: should serialize history (tiles & score)", () => {
  assertion(
    ~message = "Valid state (playing)",
    ~operator = "encodeHistoricalGameState",
    (a, b) => a == b,
    State.encodeHistoricalGameState(State.Playing({
      score: 123,
      best: Some(200),
      tiles: list{
        Tile.GameTile.createTile(
          ~id  = "tile-123",
          ~val = 4,
          ~x   = 0,
          ~y   = 1
        ),
        Tile.GameTile.createTile(
          ~id  = "tile-456",
          ~val = 2,
          ~x   = 2,
          ~y   = 2
        )
      }
    })),
    {
      let fst = Js.Json.array([
        Js.Json.string("tile-123"),
        Js.Json.string("4"),
        Js.Json.string("0"),
        Js.Json.string("1")
      ])

      let snd = Js.Json.array([
        Js.Json.string("tile-456"),
        Js.Json.string("2"),
        Js.Json.string("2"),
        Js.Json.string("2")
      ])

      Js.Json.array(Belt.Array.concat([Js.Json.string("123")], [fst, snd]))
    }
  )

  assertion(
    ~message = "Valid state (win)",
    ~operator = "encodeHistoricalGameState",
    (a, b) => a == b,
    State.encodeHistoricalGameState(State.Win({
      score: 123,
      best: Some(200),
      tiles: list{
        Tile.GameTile.createTile(
          ~id  = "tile-123",
          ~val = 2048,
          ~x   = 0,
          ~y   = 1
        ),
        Tile.GameTile.createTile(
          ~id  = "tile-456",
          ~val = 2,
          ~x   = 2,
          ~y   = 2
        )
      }
    })),
    {
      let fst = Js.Json.array([
        Js.Json.string("tile-123"),
        Js.Json.string("2048"),
        Js.Json.string("0"),
        Js.Json.string("1")
      ])

      let snd = Js.Json.array([
        Js.Json.string("tile-456"),
        Js.Json.string("2"),
        Js.Json.string("2"),
        Js.Json.string("2")
      ])

      Js.Json.array(Belt.Array.concat([Js.Json.string("123")], [fst, snd]))
    }
  )
})
