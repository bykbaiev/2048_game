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

let checkPairs = (~max: int, ~idx: int, ~expected: option<(int, int)>) =>
  assertion(
    ~message = "check pairs for equality: " ++ Js.Int.toString(max) ++ " " ++ Js.Int.toString(idx),
    ~operator = "getPair",
    (a, b) => a == b,
    Utils.getPair(max, idx),
    expected
  )

let pairs2 = [(0, 0), (0, 1), (1, 0), (1, 1)]

let pairs4 = [(0, 0), (0, 1), (0, 2), (0, 3),
              (1, 0), (1, 1), (1, 2), (1, 3),
              (2, 0), (2, 1), (2, 2), (2, 3),
              (3, 0), (3, 1), (3, 2), (3, 3)]

test("#Utils.getPair: should return correct pair from cortesian product by max value and index of this pair", () => {
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

test("#Utils.getPair: should return Nothing in case there is no such pair", () => {
  checkPairs(
    ~max = 4,
    ~idx = 20,
    ~expected = None
  )
})

test("#Utils.transpose: should transpose matrix", () => {
  assertion(
    ~message = "Transpose once",
    ~operator = "transpose",
    (a, b) => a == b,
    Utils.transpose(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 0, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } }
    }
  )

  assertion(
    ~message = "Transpose two times => nothing changes",
    ~operator = "transpose",
    (a, b) => a == b,
    Utils.transpose(
      Utils.transpose(list{
        { id: "0", val: 2, pos: { x: 0, y: 0 } },
        { id: "1", val: 4, pos: { x: 0, y: 1 } }
      })
    ),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 0, y: 1 } }
    }
  )
})

test("#Utils.reverseRow: should map all tiles in a row in reverse order", () => {
  assertion(
    ~message = "Reverse once",
    ~operator = "reverseRow",
    (a, b) => a == b,
    Utils.reverseRow(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }, 2),
    list{
      { id: "0", val: 2, pos: { x: 1, y: 0 } },
      { id: "1", val: 4, pos: { x: 0, y: 1 } }
    }
  )

  assertion(
    ~message = "reverse two times => nothing changes",
    ~operator = "reverseRow",
    (a, b) => a == b,
    Utils.reverseRow(
      Utils.reverseRow(list{
        { id: "0", val: 2, pos: { x: 0, y: 0 } },
        { id: "1", val: 4, pos: { x: 1, y: 1 } }
      }, 2),
      2
    ),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }
  )
})

test("#Utils.rotateClockwise: should rotate tiles (to handle all moves at once: move top is like to rotate anti clockwise and then move left)", () => {
  assertion(
    ~message = "Rotate once",
    ~operator = "rotateClockwise",
    (a, b) => a == b,
    Utils.rotateClockwise(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }, 2),
    list{
      { id: "0", val: 2, pos: { x: 1, y: 0 } },
      { id: "1", val: 4, pos: { x: 0, y: 1 } }
    }
  )

  assertion(
    ~message = "Rotate 4 times => nothing changes",
    ~operator = "rotateClockwise",
    (a, b) => a == b,
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    } -> Utils.rotateClockwise(2) -> Utils.rotateClockwise(2) -> Utils.rotateClockwise(2) -> Utils.rotateClockwise(2),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }
  )
})

test("#Utils.rotateAntiClockwise: should rotate tiles anti clockwise (to handle all moves at once: move top is like to rotate anti clockwise and then move left)", () => {
  assertion(
    ~message = "Rotate once",
    ~operator = "rotateAntiClockwise",
    (a, b) => a == b,
    Utils.rotateAntiClockwise(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }, 2),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 1 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } }
    }
  )

  assertion(
    ~message = "Rotate 4 times => nothing changes",
    ~operator = "rotateAntiClockwise",
    (a, b) => a == b,
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    } -> Utils.rotateAntiClockwise(2) -> Utils.rotateAntiClockwise(2) -> Utils.rotateAntiClockwise(2) -> Utils.rotateAntiClockwise(2),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }
  )
})

test("#Utils.rotateToMoveToRight: should rotate tiles to make each movement as movement to right (ease the algo)", () => {
  assertion(
    ~message = "Right (nothing changes)",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Utils.Right, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }
  )

  assertion(
    ~message = "Up",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Utils.Up, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 1, y: 0 } },
      { id: "1", val: 4, pos: { x: 0, y: 1 } }
    }
  )

  assertion(
    ~message = "Down",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Utils.Down, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 1 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } }
    }
  )

  assertion(
    ~message = "Left",
    ~operator = "rotateToMoveToRight",
    (a, b) => a == b,
    Utils.rotateToMoveToRight(2, Utils.Left, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 1, y: 1 } },
      { id: "1", val: 4, pos: { x: 0, y: 0 } }
    }
  )
})

test("#Utils.rotateBack: should rotate tiles back after rotatement to move them in specific direction", () => {
  assertion(
    ~message = "Right (nothing changes)",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Utils.Right, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }
  )

  assertion(
    ~message = "Up",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Utils.Up, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 1 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } }
    }
  )

  assertion(
    ~message = "Down",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Utils.Down, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 1, y: 0 } },
      { id: "1", val: 4, pos: { x: 0, y: 1 } }
    }
  )

  assertion(
    ~message = "Left",
    ~operator = "rotateBack",
    (a, b) => a == b,
    Utils.rotateBack(2, Utils.Left, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } }
    }),
    list{
      { id: "0", val: 2, pos: { x: 1, y: 1 } },
      { id: "1", val: 4, pos: { x: 0, y: 0 } }
    }
  )
})

test("#Utils.positionFilterPred: should verify that pair is valid & there is no such tile", () => {
  assertion(
    ~message = "Nothing won't come through me!",
    ~operator = "positionFilterPred",
    (a, b) => a == b,
    Utils.positionFilterPred(list{}, (-1, -1)),
    false
  )

  assertion(
    ~message = "Existing tile won't come through me!",
    ~operator = "positionFilterPred",
    (a, b) => a == b,
    Utils.positionFilterPred(list{{id: "0", val: 2, pos: { x: 0, y: 0 } }}, (0, 0)),
    false
  )

  assertion(
    ~message = "In other cases let's keep it",
    ~operator = "positionFilterPred",
    (a, b) => a == b,
    Utils.positionFilterPred(list{{id: "0", val: 2, pos: { x: 0, y: 0 } }}, (1, 1)),
    true
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
    Utils.isWin(list{{ id: "0", val: 2048, pos: { x: 0, y: 0 } }}),
    true
  )

  assertion(
    ~message = "In case there is no 2048 tile the game is either lost or continuing",
    ~operator = "isWin",
    (a, b) => a == b,
    Utils.isWin(list{{ id: "0", val: 1024, pos: { x: 0, y: 0 } }}),
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
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 1024, pos: { x: 1, y: 0 } },
      { id: "2", val: 1024, pos: { x: 0, y: 1 } },
      { id: "3", val: 2, pos: { x: 1, y: 1 } }
    }),
    true
  )

  assertion(
    ~message = "If there are all possible tiles are used but they can still be collapsed the game is not lost yet",
    ~operator = "isLoss",
    (a, b) => a == b,
    Utils.isLoss(2, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 2, pos: { x: 1, y: 0 } },
      { id: "2", val: 1024, pos: { x: 0, y: 1 } },
      { id: "3", val: 2, pos: { x: 1, y: 1 } }
    }),
    false
  )

  assertion(
    ~message = "In case there is no 2048 tile the game is either lost or continuing",
    ~operator = "isLoss",
    (a, b) => a == b,
    Utils.isLoss(2, list{{ id: "0", val: 1024, pos: { x: 0, y: 0 } }}),
    false
  )
})

test("#Utils.isMoveToRightPossible: should check if it's possible to move tiles to the right", () => {
  assertion(
    ~message = "One row is empty and other is full",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } },
    }, 2),
    false
  )

  assertion(
    ~message = "Possible",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 2, pos: { x: 0, y: 1 } },
    }, 2),
    true
  )

  assertion(
    ~message = "Possible (with collapsing)",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 2, pos: { x: 1, y: 0 } },
    }, 2),
    true
  )

  assertion(
    ~message = "Impossible (3 * 3)",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } },
      { id: "2", val: 2, pos: { x: 2, y: 0 } },
      { id: "3", val: 2, pos: { x: 1, y: 1 } },
      { id: "4", val: 4, pos: { x: 2, y: 1 } },
    }, 3),
    false
  )

  assertion(
    ~message = "Possible (3 * 3)",
    ~operator = "isMoveToRightPossible",
    (a, b) => a == b,
    Utils.isMoveToRightPossible(list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } },
      { id: "2", val: 2, pos: { x: 2, y: 0 } },
      { id: "3", val: 2, pos: { x: 1, y: 1 } },
      { id: "4", val: 2, pos: { x: 2, y: 1 } },
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
      { id: "0", val: 2,  pos: { x: 2, y: 0 } },
      { id: "1", val: 16, pos: { x: 0, y: 2 } },
      { id: "2", val: 4,  pos: { x: 1, y: 0 } },
      { id: "3", val: 32, pos: { x: 1, y: 1 } },
      { id: "4", val: 8,  pos: { x: 0, y: 0 } },
    }),
    list{
      { id: "4", val: 8,  pos: { x: 0, y: 0 } },
      { id: "2", val: 4,  pos: { x: 1, y: 0 } },
      { id: "0", val: 2,  pos: { x: 2, y: 0 } },
      { id: "3", val: 32, pos: { x: 1, y: 1 } },
      { id: "1", val: 16, pos: { x: 0, y: 2 } },
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
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } },
    }),
    list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 0 } },
    }
  )

  assertion(
    ~message = "In case some tiles can be moved they should be moved",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(2, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 4, pos: { x: 0, y: 1 } },
    }),
    list{
      { id: "0", val: 2, pos: { x: 1, y: 0 } },
      { id: "1", val: 4, pos: { x: 1, y: 1 } },
    }
  )

  assertion(
    ~message = "In case some tiles can be moved they should be moved (3 * 3)",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(3, list{
      { id: "0", val: 2,  pos: { x: 1, y: 1 } },
      { id: "1", val: 4,  pos: { x: 0, y: 0 } },
      { id: "2", val: 8,  pos: { x: 0, y: 1 } },
      { id: "3", val: 16, pos: { x: 2, y: 0 } },
      { id: "4", val: 32, pos: { x: 2, y: 2 } },
    }),
    list{
      { id: "0", val: 2,  pos: { x: 2, y: 1 } },
      { id: "1", val: 4,  pos: { x: 1, y: 0 } },
      { id: "2", val: 8,  pos: { x: 1, y: 1 } },
      { id: "3", val: 16, pos: { x: 2, y: 0 } },
      { id: "4", val: 32, pos: { x: 2, y: 2 } },
    }
  )

  assertion(
    ~message = "In case some tiles can be collapsed they should be collapsed",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(2, list{
      { id: "0", val: 2, pos: { x: 0, y: 0 } },
      { id: "1", val: 2, pos: { x: 1, y: 0 } },
    }),
    list{
      { id: "0", val: 4, pos: { x: 1, y: 0 } },
    }
  )

  assertion(
    ~message = "In case some tiles can be collapsed they should be collapsed (3 * 3)",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(3, list{
      { id: "0", val: 2,  pos: { x: 1, y: 1 } },
      { id: "1", val: 4,  pos: { x: 0, y: 0 } },
      { id: "2", val: 8,  pos: { x: 0, y: 1 } },
      { id: "3", val: 4,  pos: { x: 2, y: 0 } },
      { id: "4", val: 32, pos: { x: 2, y: 2 } },
    }),
    list{
      { id: "0", val: 2,  pos: { x: 2, y: 1 } },
      { id: "1", val: 8,  pos: { x: 2, y: 0 } },
      { id: "2", val: 8,  pos: { x: 1, y: 1 } },
      { id: "4", val: 32, pos: { x: 2, y: 2 } },
    }
  )

  assertion(
    ~message = "In case some tiles can be collapsed they should be collapsed (4 * 4)",
    ~operator = "moveRight",
    compareTiles,
    Utils.moveRight(4, list{
      { id: "0", val: 2,  pos: { x: 1, y: 1 } },
      { id: "1", val: 4,  pos: { x: 0, y: 0 } },
      { id: "2", val: 8,  pos: { x: 0, y: 1 } },
      { id: "3", val: 4,  pos: { x: 2, y: 0 } },
      { id: "4", val: 32, pos: { x: 2, y: 2 } },
      { id: "5", val: 4,  pos: { x: 0, y: 3 } },
      { id: "6", val: 4,  pos: { x: 1, y: 3 } },
      { id: "7", val: 4,  pos: { x: 2, y: 3 } },
      { id: "8", val: 4,  pos: { x: 3, y: 3 } },
    }),
    list{
      { id: "0", val: 2,  pos: { x: 3, y: 1 } },
      { id: "1", val: 8,  pos: { x: 3, y: 0 } },
      { id: "2", val: 8,  pos: { x: 2, y: 1 } },
      { id: "4", val: 32, pos: { x: 3, y: 2 } },
      { id: "5", val: 8,  pos: { x: 2, y: 3 } },
      { id: "7", val: 8,  pos: { x: 3, y: 3 } },
    }
  )
})
