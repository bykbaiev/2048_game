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

      Js.Json.array(Belt.Array.concat([Js.Json.string("123"), Js.Json.string("playing")], [fst, snd]))
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

      Js.Json.array(Belt.Array.concat([Js.Json.string("123"), Js.Json.string("win")], [fst, snd]))
    }
  )
})

test("#State.decodeHistoricalGameState: should deserialize history", () => {
  assertion(
    ~message = "Invalid state",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(None),
    None
  )

  assertion(
    ~message = "Invalid state (2)",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(Some("[]")),
    None
  )

  assertion(
    ~message = "Invalid state (3)",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(Some("[\"asdf\"]")),
    None
  )

  assertion(
    ~message = "Invalid state (4)",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(Some("[\"123\", \"asdf\"]")),
    None
  )

  assertion(
    ~message = "Invalid state (5)",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(Some("[\"123\", \"win\", []]")),
    None
  )

  assertion(
    ~message = "~ Valid state (without tiles)",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(Some("[\"123\", \"playing\"]")),
    Some(State.Playing({
      best: None,
      score: 123,
      tiles: list{}
    }))
  )

  assertion(
    ~message = "~ Valid state (with tiles)",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(Some("[\"123\", \"playingAfterWin\", [\"tile-123\", \"2048\", \"0\", \"3\"]]")),
    Some(State.PlayingAfterWin({
      best: None,
      score: 123,
      tiles: list{
        Tile.GameTile.Converters.toAverage(
          Tile.GameTile.createTile(
            ~id  = "tile-123",
            ~val = 2048,
            ~x   = 0,
            ~y   = 3
          )
        )
      }
    }))
  )

  assertion(
    ~message = "back and forth",
    ~operator = "decodeHistoricalGameState",
    (a, b) => a == b,
    State.decodeHistoricalGameState(
      Some(
        Js.Json.stringify(
          State.encodeHistoricalGameState(
            State.PlayingAfterWin({
              best: None,
              score: 123,
              tiles: list{
                Tile.GameTile.Converters.toAverage(
                  Tile.GameTile.createTile(
                    ~id  = "tile-123",
                    ~val = 2048,
                    ~x   = 0,
                    ~y   = 3
                  )
                )
              }
            })
          )
        )
      )
    ),
    Some(State.PlayingAfterWin({
      best: None,
      score: 123,
      tiles: list{
        Tile.GameTile.Converters.toAverage(
          Tile.GameTile.createTile(
            ~id  = "tile-123",
            ~val = 2048,
            ~x   = 0,
            ~y   = 3
          )
        )
      }
    }))
  )
})

test("#State.getBestScoreOfStates", () => {
  assertion(
    ~message = "Both are None",
    ~operator = "getBestScoreOfStates",
    (a, b) => a === b,
    State.getBestScoreOfStates(
      State.Playing({ best: None, score: 123, tiles: list{} }),
      State.Playing({ best: None, score: 456, tiles: list{} })
    ),
    "456"
  )

  assertion(
    ~message = "Old is None",
    ~operator = "getBestScoreOfStates",
    (a, b) => a === b,
    State.getBestScoreOfStates(
      State.Playing({ best: None,      score: 123, tiles: list{} }),
      State.Playing({ best: Some(789), score: 456, tiles: list{} })
    ),
    "789"
  )

  assertion(
    ~message = "New is None",
    ~operator = "getBestScoreOfStates",
    (a, b) => a === b,
    State.getBestScoreOfStates(
      State.Playing({ best: Some(789), score: 123, tiles: list{} }),
      State.Playing({ best: None,      score: 456, tiles: list{} })
    ),
    "789"
  )

  assertion(
    ~message = "Both are Some",
    ~operator = "getBestScoreOfStates",
    (a, b) => a === b,
    State.getBestScoreOfStates(
      State.Playing({ best: Some(789), score: 123, tiles: list{} }),
      State.Playing({ best: Some(655),      score: 456, tiles: list{} })
    ),
    "789"
  )
})

test("#State.decodeHistory", () => {
  assertion(
    ~message = "Missing value",
    ~operator = "decodeHistory",
    (a, b) => a == b,
    State.decodeHistory(None),
    None
  )

  assertion(
    ~message = "Missing value (2)",
    ~operator = "decodeHistory",
    (a, b) => a == b,
    State.decodeHistory(Some("")),
    None
  )

  assertion(
    ~message = "Missing history",
    ~operator = "decodeHistory",
    (a, b) => a == b,
    State.decodeHistory(Some("[]")),
    Some([])
  )

  assertion(
    ~message = "Missing history",
    ~operator = "decodeHistory",
    (a, b) => a == b,
    State.decodeHistory(Some("[\"doesn't matter what is inside\"]")),
    Some([Js.Json.string("doesn't matter what is inside")])
  )
})

test("#State.rollbackState", () => {
  let basicState = State.Playing({
    best: Some(100),
    score: 100,
    tiles: list{
      Tile.GameTile.createTile(
        ~id  = "tile-123",
        ~val = 4,
        ~x   = 3,
        ~y   = 1
      )
    }
  })

  let previousState = State.Playing({
    best: Some(96),
    score: 96,
    tiles: list{
      Tile.GameTile.createTile(
        ~id  = "tile-456",
        ~val = 2,
        ~x   = 0,
        ~y   = 1
      ) -> Tile.GameTile.Converters.toAverage,
      Tile.GameTile.createTile(
        ~id  = "tile-123",
        ~val = 2,
        ~x   = 2,
        ~y   = 1
      ) -> Tile.GameTile.Converters.toAverage
    }
  })

  assertion(
    ~message = "No previous historical records",
    ~operator = "rollbackState",
    (a, b) => a == b,
    State.rollbackState(Js.Json.string(""), basicState),
    basicState
  )

  assertion(
    ~message = "There was previous state",
    ~operator = "rollbackState",
    (a, b) => a == b,
    State.rollbackState(State.encodeHistoricalGameState(previousState), basicState),
    State.setInternals(previousState, State.setBestScore(State.getInternals(previousState), Some(100)))
  )
})

test("#State.updateHistory", () => {
  let state = State.Playing({
    score: 123,
    best: Some(123),
    tiles: list{
      Tile.GameTile.createTile(
        ~id  = "tile-456",
        ~val = 2,
        ~x   = 0,
        ~y   = 1
      ) -> Tile.GameTile.Converters.toAverage,
      Tile.GameTile.createTile(
        ~id  = "tile-123",
        ~val = 2,
        ~x   = 2,
        ~y   = 1
      ) -> Tile.GameTile.Converters.toAverage
    }
  })

  let prev = State.Playing({
    score: 120,
    best: Some(120),
    tiles: list{
      Tile.GameTile.createTile(
        ~id  = "tile-123",
        ~val = 2,
        ~x   = 2,
        ~y   = 1
      ) -> Tile.GameTile.Converters.toAverage
    }
  })

  assertion(
    ~message = "Nothing changed => the state has already been added to the history",
    ~operator = "updateHistory",
    (a, b) => a == b,
    State.updateHistory(state, [State.encodeHistoricalGameState(state)]),
    [State.encodeHistoricalGameState(state)]
  )

  assertion(
    ~message = "The state is different from what is in history",
    ~operator = "updateHistory",
    (a, b) => a == b,
    State.updateHistory(state, [State.encodeHistoricalGameState(prev)]),
    [State.encodeHistoricalGameState(prev), State.encodeHistoricalGameState(state)]
  )

  let xs = Belt.Array.makeBy(15, idx => idx -> Belt.Int.toString -> Js.Json.string)

  assertion(
    ~message = "Should keep only 15 last states in history",
    ~operator = "updateHistory",
    (a, b) => a == b,
    State.updateHistory(state, xs),
    Belt.Array.concat(Belt.Array.sliceToEnd(xs, 1), [State.encodeHistoricalGameState(state)])
  )
})
