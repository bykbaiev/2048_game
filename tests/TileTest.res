open Test

type testPos = {
  x: int,
  y: int
}

type testTile = {
  id: string,
  merged: bool,
  val: int,
  pos: testPos,
}

let createTestTile = ({ id, val, pos, merged }): Tile.GameTile.tile => {
  let average = Tile.GameTile.createTile(
                  ~id  = id,
                  ~val = val,
                  ~x   = pos.x,
                  ~y   = pos.y
                ) -> Tile.GameTile.Converters.toAverage
  if merged {
    Tile.GameTile.Converters.toMerged(average)
  } else {
    average
  }
}

let checkPairs = (~max: int, ~idx: int, ~expected: option<(int, int)>) =>
  assertion(
    ~message = "check pairs for equality: " ++ Js.Int.toString(max) ++ " " ++ Js.Int.toString(idx),
    ~operator = "getPair",
    (a, b) => a == b,
    Tile.GameTile.getPair(max, idx),
    expected
  )

let pairs2 = [(0, 0), (0, 1), (1, 0), (1, 1)]

let pairs4 = [(0, 0), (0, 1), (0, 2), (0, 3),
              (1, 0), (1, 1), (1, 2), (1, 3),
              (2, 0), (2, 1), (2, 2), (2, 3),
              (3, 0), (3, 1), (3, 2), (3, 3)]

test("#Tile.getPair: should return correct pair from cortesian product by max value and index of this pair", () => {
  checkPairs(
    ~max = 2,
    ~idx = 0,
    ~expected = Some(pairs2[0])
  )
  checkPairs(
    ~max = 2,
    ~idx = 1,
    ~expected = Some(pairs2[1])
  )
  checkPairs(
    ~max = 2,
    ~idx = 2,
    ~expected = Some(pairs2[2])
  )
  checkPairs(
    ~max = 2,
    ~idx = 3,
    ~expected = Some(pairs2[3])
  )
  checkPairs(
    ~max = 4,
    ~idx = 0,
    ~expected = Some(pairs4[0])
  )
  checkPairs(
    ~max = 4,
    ~idx = 1,
    ~expected = Some(pairs4[1])
  )
  checkPairs(
    ~max = 4,
    ~idx = 2,
    ~expected = Some(pairs4[2])
  )
  checkPairs(
    ~max = 4,
    ~idx = 3,
    ~expected = Some(pairs4[3])
  )
  checkPairs(
    ~max = 4,
    ~idx = 4,
    ~expected = Some(pairs4[4])
  )
  checkPairs(
    ~max = 4,
    ~idx = 5,
    ~expected = Some(pairs4[5])
  )
  checkPairs(
    ~max = 4,
    ~idx = 6,
    ~expected = Some(pairs4[6])
  )
  checkPairs(
    ~max = 4,
    ~idx = 7,
    ~expected = Some(pairs4[7])
  )
  checkPairs(
    ~max = 4,
    ~idx = 8,
    ~expected = Some(pairs4[8])
  )
  checkPairs(
    ~max = 4,
    ~idx = 9,
    ~expected = Some(pairs4[9])
  )
  checkPairs(
    ~max = 4,
    ~idx = 10,
    ~expected = Some(pairs4[10])
  )
  checkPairs(
    ~max = 4,
    ~idx = 15,
    ~expected = Some(pairs4[15])
  )
})

test("#Tile.getPair: should return Nothing in case there is no such pair", () => {
  checkPairs(
    ~max = 4,
    ~idx = 20,
    ~expected = None
  )
})

test("#Tile.positionFilterPred: should verify that pair is valid & there is no such tile", () => {
  assertion(
    ~message = "Nothing won't come through me!",
    ~operator = "positionFilterPred",
    (a, b) => a == b,
    Tile.GameTile.positionFilterPred(list{}, (-1, -1)),
    false
  )

  assertion(
    ~message = "Existing tile won't come through me!",
    ~operator = "positionFilterPred",
    (a, b) => a == b,
    Tile.GameTile.positionFilterPred(
      list{
        createTestTile({id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } })
      },
      (0, 0)
    ),
    false
  )

  assertion(
    ~message = "In other cases let's keep it",
    ~operator = "positionFilterPred",
    (a, b) => a == b,
    Tile.GameTile.positionFilterPred(
      list{
        createTestTile({id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } })
      },
      (1, 1)
    ),
    true
  )
})

test("#Tile.encode: should encode the tile to keep the history", () => {
  assertion(
    ~message = "Average tile",
    ~operator = "encode",
    (a, b) => a == b,
    Tile.GameTile.encode(
      createTestTile({id: "0", merged: false, val: 2, pos: { x: 0, y: 1 } })
    ),
    {
      let dict = Js.Dict.empty()

      Js.Dict.set(dict, "status", Js.Json.string("average"))
      Js.Dict.set(dict, "id", Js.Json.string("0"))
      Js.Dict.set(dict, "val", Js.Json.number(2.))
      Js.Dict.set(dict, "x", Js.Json.number(0.))
      Js.Dict.set(dict, "y", Js.Json.number(1.))

      Js.Json.object_(dict)
    }
  )

  assertion(
    ~message = "Merged tile",
    ~operator = "encode",
    (a, b) => a == b,
    Tile.GameTile.encode(
      createTestTile({id: "0", merged: true, val: 2, pos: { x: 0, y: 1 } })
    ),
    {
      let dict = Js.Dict.empty()

      Js.Dict.set(dict, "status", Js.Json.string("merged"))
      Js.Dict.set(dict, "id", Js.Json.string("0"))
      Js.Dict.set(dict, "val", Js.Json.number(2.))
      Js.Dict.set(dict, "x", Js.Json.number(0.))
      Js.Dict.set(dict, "y", Js.Json.number(1.))

      Js.Json.object_(dict)
    }
  )

  assertion(
    ~message = "New tile",
    ~operator = "encode",
    (a, b) => a == b,
    Tile.GameTile.encode(
      Tile.GameTile.Converters.toNew(createTestTile({id: "0", merged: false, val: 2, pos: { x: 0, y: 1 } }))
    ),
    {
      let dict = Js.Dict.empty()

      Js.Dict.set(dict, "status", Js.Json.string("new"))
      Js.Dict.set(dict, "id", Js.Json.string("0"))
      Js.Dict.set(dict, "val", Js.Json.number(2.))
      Js.Dict.set(dict, "x", Js.Json.number(0.))
      Js.Dict.set(dict, "y", Js.Json.number(1.))

      Js.Json.object_(dict)
    }
  )
})

test("#Tile.decode: should decode the tile JSON representation to actual tile", () => {
  assertion(
    ~message = "Invalid tile",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.decode(Js.Json.parseExn("{}")),
    None
  )

  assertion(
    ~message = "Invalid tile (2)",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.decode(Js.Json.parseExn("{\"status\": \"non-existing\"}")),
    None
  )

  assertion(
    ~message = "Invalid tile (3)",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.decode(Js.Json.parseExn("{\"status\": \"merged\"}")),
    None
  )

  assertion(
    ~message = "Average tile",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.decode(Js.Json.parseExn("{\"status\": \"average\", \"id\": \"tile-123\", \"val\": 4, \"x\": 0, \"y\": 1 }")),
    Some(createTestTile({id: "tile-123", merged: false, val: 4, pos: { x: 0, y: 1 } }))
  )

  assertion(
    ~message = "Merged tile",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.decode(Js.Json.parseExn("{\"status\": \"merged\", \"id\": \"tile-123\", \"val\": 4, \"x\": 0, \"y\": 1 }")),
    Some(createTestTile({id: "tile-123", merged: true, val: 4, pos: { x: 0, y: 1 } }))
  )

  assertion(
    ~message = "New tile",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.decode(Js.Json.parseExn("{\"status\": \"new\", \"id\": \"tile-123\", \"val\": 4, \"x\": 0, \"y\": 1 }")),
    Some(
      Tile.GameTile.Converters.toNew(
        createTestTile({id: "tile-123", merged: false, val: 4, pos: { x: 0, y: 1 } })
      )
    )
  )
})

test("#Tile.encodeHistorical: dumb tile representation for history", () => {
  assertion(
    ~message = "New tile",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.encodeHistorical(
      createTestTile({id: "tile-1", merged: false, val: 2, pos: { x: 0, y: 1 } })
    ),
    {
      let tile = [
        Js.Json.string("tile-1"),
        Js.Json.string("2"),
        Js.Json.string("0"),
        Js.Json.string("1")
      ]
      Js.Json.array(tile)
    }
  )

  assertion(
    ~message = "Merged tile",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.encodeHistorical(
      createTestTile({id: "tile-1", merged: true, val: 2, pos: { x: 0, y: 1 } })
    ),
    {
      let tile = [
        Js.Json.string("tile-1"),
        Js.Json.string("2"),
        Js.Json.string("0"),
        Js.Json.string("1")
      ]
      Js.Json.array(tile)
    }
  )

  assertion(
    ~message = "Average tile",
    ~operator = "decode",
    (a, b) => a == b,
    Tile.GameTile.encodeHistorical(
      Tile.GameTile.Converters.toAverage(
        createTestTile({
          id: "tile-1",
          merged: false,
          val: 2,
          pos: { x: 0, y: 1 }
        })
      )
    ),
    {
      let tile = [
        Js.Json.string("tile-1"),
        Js.Json.string("2"),
        Js.Json.string("0"),
        Js.Json.string("1")
      ]
      Js.Json.array(tile)
    }
  )
})

test("#Tile.decodeHistorical: tile from it's dumb representation", () => {
  assertion(
    ~message = "Invalid tile",
    ~operator = "decodeHistorical",
    (a, b) => a == b,
    Tile.GameTile.decodeHistorical(Js.Json.parseExn("[]")),
    None
  )

  assertion(
    ~message = "Invalid tile (2)",
    ~operator = "decodeHistorical",
    (a, b) => a == b,
    Tile.GameTile.decodeHistorical(Js.Json.parseExn("[0, 2, \"asdf\", 3]")),
    None
  )

  assertion(
    ~message = "Invalid tile (3)",
    ~operator = "decodeHistorical",
    (a, b) => a == b,
    Tile.GameTile.decodeHistorical(Js.Json.parseExn("[\"0\", 2, 3]")),
    None
  )

  assertion(
    ~message = "Valid tile",
    ~operator = "decodeHistorical",
    (a, b) => a == b,
    Tile.GameTile.decodeHistorical(
      Js.Json.parseExn(
        "[\"tile-123\", 2, 3, 3]"
      )
    ),
    Some(
      {id: "tile-123", merged: false, val: 2, pos: { x: 3, y: 3 } }
        -> createTestTile
        -> Tile.GameTile.Converters.toAverage
    )
  )
})
