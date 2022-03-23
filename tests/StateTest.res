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
