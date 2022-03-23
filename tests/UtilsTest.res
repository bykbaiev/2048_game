open Test

let styles = Js.Dict.fromArray([
  ("default", Js.Dict.fromArray([
    ("testSmth", "testSmthCls"),
    ("test-1", "test-1-cls")
  ]))
])

let checkCls = (~message: string, ~name: string, ~expected: string) =>
  assertion(
    ~message = message,
    ~operator = "getCls",
    (a, b) => a === b,
    Utils.getCls(styles, name),
    expected
  )

test("#Utils.getCls", () => {
  checkCls(
    ~message = "non existing keys return empty strings",
    ~name = "non-existing-key",
    ~expected = ""
  )
})

test("#Utils.getCls", () => {
  checkCls(
    ~message = "camel-cased strings are processed correctly",
    ~name = "testSmth",
    ~expected = "testSmthCls"
  )
})

test("#Utils.getCls", () => {
  checkCls(
    ~message = "kebab-cased strings are processed correctly",
    ~name = "test-1",
    ~expected = "test-1-cls"
  )
})

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

test("#Utils.transpose: should transpose matrix", () => {
  assertion(
    ~message = "Transpose once",
    ~operator = "transpose",
    (a, b) => a == b,
    Utils.transpose(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } })
    }
  )

  assertion(
    ~message = "Transpose two times => nothing changes",
    ~operator = "transpose",
    (a, b) => a == b,
    Utils.transpose(
      Utils.transpose(list{
        createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
        createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } })
      })
    ),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } })
    }
  )
})

test("#Utils.reverseRow: should map all tiles in a row in reverse order", () => {
  assertion(
    ~message = "Reverse once",
    ~operator = "reverseRow",
    (a, b) => a == b,
    Utils.reverseRow(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }, 2),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } })
    }
  )

  assertion(
    ~message = "reverse two times => nothing changes",
    ~operator = "reverseRow",
    (a, b) => a == b,
    Utils.reverseRow(
      Utils.reverseRow(list{
        createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
        createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
      }, 2),
      2
    ),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }
  )
})

test("#Utils.rotateClockwise: should rotate tiles (to handle all moves at once: move top is like to rotate anti clockwise and then move left)", () => {
  assertion(
    ~message = "Rotate once",
    ~operator = "rotateClockwise",
    (a, b) => a == b,
    Utils.rotateClockwise(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }, 2),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } })
    }
  )

  assertion(
    ~message = "Rotate 4 times => nothing changes",
    ~operator = "rotateClockwise",
    (a, b) => a == b,
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    } -> Utils.rotateClockwise(2) -> Utils.rotateClockwise(2) -> Utils.rotateClockwise(2) -> Utils.rotateClockwise(2),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }
  )
})

test("#Utils.rotateAntiClockwise: should rotate tiles anti clockwise (to handle all moves at once: move top is like to rotate anti clockwise and then move left)", () => {
  assertion(
    ~message = "Rotate once",
    ~operator = "rotateAntiClockwise",
    (a, b) => a == b,
    Utils.rotateAntiClockwise(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }, 2),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } })
    }
  )

  assertion(
    ~message = "Rotate 4 times => nothing changes",
    ~operator = "rotateAntiClockwise",
    (a, b) => a == b,
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    } -> Utils.rotateAntiClockwise(2) -> Utils.rotateAntiClockwise(2) -> Utils.rotateAntiClockwise(2) -> Utils.rotateAntiClockwise(2),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }
  )
})

test("#Utils.rotateToMoveToRight: should rotate tiles to make each movement as movement to right (ease the algo)", () => {
  assertion(
    ~message = "Right (nothing changes)",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Constants.Right, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }
  )

  assertion(
    ~message = "Up",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Constants.Up, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } })
    }
  )

  assertion(
    ~message = "Down",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Constants.Down, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } })
    }
  )

  assertion(
    ~message = "Left",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Constants.Left, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 0 } })
    }
  )
})

test("#Utils.rotateBack: should rotate tiles back after rotatement to move them in specific direction", () => {
  assertion(
    ~message = "Right (nothing changes)",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Constants.Right, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }
  )

  assertion(
    ~message = "Up",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Constants.Up, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } })
    }
  )

  assertion(
    ~message = "Down",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Constants.Down, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } })
    }
  )

  assertion(
    ~message = "Left",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Constants.Left, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } })
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 0 } })
    }
  )
})

test("#Utils.keyCodeToDirection: should map integer code to direction constructor", () => {
  assertion(
    ~message = "Invalid code won't come through me!",
    ~operator = "keyCodeToDirection",
    (a, b) => a == b,
    Utils.keyCodeToDirection(123),
    None
  )

  assertion(
    ~message = "Direction will be greated!",
    ~operator = "keyCodeToDirection",
    (a, b) => a == b,
    Utils.keyCodeToDirection(40),
    Some(Down)
  )
})

test("#Utils.isWin: should check if user won", () => {
  assertion(
    ~message = "For win there should be at least one tile",
    ~operator = "isWin",
    (a, b) => a == b,
    Utils.isWin(list{}),
    false
  )

  assertion(
    ~message = "Win is when there is one tile with value 2048",
    ~operator = "isWin",
    (a, b) => a == b,
    Utils.isWin(list{
      createTestTile(
        { id: "0", merged: false, val: 2048, pos: { x: 0, y: 0 } }
      )
    }),
    true
  )

  assertion(
    ~message = "In case there is no 2048 tile the game is either lost or continuing",
    ~operator = "isWin",
    (a, b) => a == b,
    Utils.isWin(list{
      createTestTile({ id: "0", merged: false, val: 1024, pos: { x: 0, y: 0 } })
    }),
    false
  )
})

test("#Utils.isLoss: should check if user loss", () => {
  assertion(
    ~message = "For loss tiles should not be empty",
    ~operator = "isLoss",
    (a, b) => a == b,
    Utils.isLoss(2, list{}),
    false
  )

  assertion(
    ~message = "Loss is when the number of tiles is the same as max size and there are no 2048 tile or collapsed tiles",
    ~operator = "isLoss",
    (a, b) => a == b,
    Utils.isLoss(2, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 1024, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 1024, pos: { x: 0, y: 1 } }),
      createTestTile({ id: "3", merged: false, val: 2, pos: { x: 1, y: 1 } })
    }),
    true
  )

  assertion(
    ~message = "If there are all possible tiles are used but they can still be collapsed the game is not lost yet",
    ~operator = "isLoss",
    (a, b) => a == b,
    Utils.isLoss(2, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 2, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 1024, pos: { x: 0, y: 1 } }),
      createTestTile({ id: "3", merged: false, val: 2, pos: { x: 1, y: 1 } })
    }),
    false
  )

  assertion(
    ~message = "In case there is no 2048 tile the game is either lost or continuing",
    ~operator = "isLoss",
    (a, b) => a == b,
    Utils.isLoss(2, list{
      createTestTile(
        { id: "0", merged: false, val: 1024, pos: { x: 0, y: 0 } }
      )}),
    false
  )
})

test("#Utils.isMoveToRightPossible: should check if it's possible to move tiles to the right", () => {
  assertion(
    ~message = "One row is empty and other is full",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } }),
    }, 2),
    false
  )

  assertion(
    ~message = "Possible",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 2, pos: { x: 0, y: 1 } }),
    }, 2),
    true
  )

  assertion(
    ~message = "Possible (with collapsing)",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 2, pos: { x: 1, y: 0 } }),
    }, 2),
    true
  )

  assertion(
    ~message = "Impossible (3 * 3)",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 2, pos: { x: 2, y: 0 } }),
      createTestTile({ id: "3", merged: false, val: 2, pos: { x: 1, y: 1 } }),
      createTestTile({ id: "4", merged: false, val: 4, pos: { x: 2, y: 1 } }),
    }, 3),
    false
  )

  assertion(
    ~message = "Possible (3 * 3)",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 2, pos: { x: 2, y: 0 } }),
      createTestTile({ id: "3", merged: false, val: 2, pos: { x: 1, y: 1 } }),
      createTestTile({ id: "4", merged: false, val: 2, pos: { x: 2, y: 1 } }),
    }, 3),
    true
  )
})

test("#Utils.sortTilesByColumn: should sort all tiles in one row by column number in ascending order", () => {
  assertion(
    ~message = "should sort",
    ~operator = "sortTilesByColumn",
    (a, b) => a == b,
    Utils.sortTilesByColumn(list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 2, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 16, pos: { x: 0, y: 2 } }),
      createTestTile({ id: "2", merged: false, val: 4,  pos: { x: 1, y: 0 } }),
      createTestTile({ id: "3", merged: false, val: 32, pos: { x: 1, y: 1 } }),
      createTestTile({ id: "4", merged: false, val: 8,  pos: { x: 0, y: 0 } }),
    }),
    list{
      createTestTile({ id: "4", merged: false, val: 8,  pos: { x: 0, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 4,  pos: { x: 1, y: 0 } }),
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 2, y: 0 } }),
      createTestTile({ id: "3", merged: false, val: 32, pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 16, pos: { x: 0, y: 2 } }),
    }
  )
})

let compareTiles = (xs, ys) => {
  Belt.List.size(xs) === Belt.List.size(ys) && Belt.List.every(xs, tile => Belt.Option.mapWithDefault(
    Belt.List.getBy(ys, t => t == tile),
    false,
    _ => true
  ))
}

test("#Utils.moveRight: should move all tiles to right where it's possible", () => {
  assertion(
    ~message = "In case nothing can be moved nothing should be changed",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(2, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } }),
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 0 } }),
    }
  )

  assertion(
    ~message = "In case some tiles can be moved they should be moved",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(2, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 0, y: 1 } }),
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 1, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 4, pos: { x: 1, y: 1 } }),
    }
  )

  assertion(
    ~message = "In case some tiles can be moved they should be moved (3 * 3)",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(3, list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4,  pos: { x: 0, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 8,  pos: { x: 0, y: 1 } }),
      createTestTile({ id: "3", merged: false, val: 16, pos: { x: 2, y: 0 } }),
      createTestTile({ id: "4", merged: false, val: 32, pos: { x: 2, y: 2 } }),
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 2, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4,  pos: { x: 1, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 8,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "3", merged: false, val: 16, pos: { x: 2, y: 0 } }),
      createTestTile({ id: "4", merged: false, val: 32, pos: { x: 2, y: 2 } }),
    }
  )

  assertion(
    ~message = "In case some tiles can be collapsed they should be collapsed",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(2, list{
      createTestTile({ id: "0", merged: false, val: 2, pos: { x: 0, y: 0 } }),
      createTestTile({ id: "1", merged: false, val: 2, pos: { x: 1, y: 0 } }),
    }),
    list{
      createTestTile({ id: "0", merged: true,  val: 4, pos: { x: 1, y: 0 } }),
    }
  )

  assertion(
    ~message = "In case some tiles can be collapsed they should be collapsed (3 * 3)",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(3, list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4,  pos: { x: 0, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 8,  pos: { x: 0, y: 1 } }),
      createTestTile({ id: "3", merged: false, val: 4,  pos: { x: 2, y: 0 } }),
      createTestTile({ id: "4", merged: false, val: 32, pos: { x: 2, y: 2 } }),
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 2, y: 1 } }),
      createTestTile({ id: "1", merged: true,  val: 8,  pos: { x: 2, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 8,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "4", merged: false, val: 32, pos: { x: 2, y: 2 } }),
    }
  )

  assertion(
    ~message = "In case some tiles can be collapsed they should be collapsed (4 * 4)",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(4, list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4,  pos: { x: 0, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 8,  pos: { x: 0, y: 1 } }),
      createTestTile({ id: "3", merged: false, val: 4,  pos: { x: 2, y: 0 } }),
      createTestTile({ id: "4", merged: false, val: 32, pos: { x: 2, y: 2 } }),
      createTestTile({ id: "5", merged: false, val: 4,  pos: { x: 0, y: 3 } }),
      createTestTile({ id: "6", merged: false, val: 4,  pos: { x: 1, y: 3 } }),
      createTestTile({ id: "7", merged: false, val: 4,  pos: { x: 2, y: 3 } }),
      createTestTile({ id: "8", merged: false, val: 4,  pos: { x: 3, y: 3 } }),
    }),
    list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 3, y: 1 } }),
      createTestTile({ id: "1", merged: true,  val: 8,  pos: { x: 3, y: 0 } }),
      createTestTile({ id: "2", merged: false, val: 8,  pos: { x: 2, y: 1 } }),
      createTestTile({ id: "4", merged: false, val: 32, pos: { x: 3, y: 2 } }),
      createTestTile({ id: "5", merged: true,  val: 8,  pos: { x: 2, y: 3 } }),
      createTestTile({ id: "7", merged: true,  val: 8,  pos: { x: 3, y: 3 } }),
    }
  )
})

test("#Utils.recalculateScore: should add values of new merged tiles to previous score", () => {
  assertion(
    ~message = "In case no 2 nodes were merged the score is the same as before",
    ~operator = "recalculateScore",
    (a, b) => a === b,
    Utils.recalculateScore(list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: false, val: 4,  pos: { x: 0, y: 0 } }),
    }, 20),
    20
  )

  assertion(
    ~message = "if there is a merged tile score should be updated",
    ~operator = "recalculateScore",
    (a, b) => a === b,
    Utils.recalculateScore(list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: true, val: 4,  pos: { x: 0, y: 0 } }),
    }, 20),
    20 + 4
  )

  assertion(
    ~message = "if there are multiple merged tiles score should be updated",
    ~operator = "recalculateScore",
    (a, b) => a === b,
    Utils.recalculateScore(list{
      createTestTile({ id: "0", merged: false, val: 2,  pos: { x: 1, y: 1 } }),
      createTestTile({ id: "1", merged: true, val: 4,   pos: { x: 0, y: 0 } }),
      createTestTile({ id: "2", merged: true, val: 32,  pos: { x: 0, y: 0 } }),
      createTestTile({ id: "3", merged: true, val: 16,  pos: { x: 0, y: 0 } }),
    }, 20),
    20 + 4 + 32 + 16
  )
})
