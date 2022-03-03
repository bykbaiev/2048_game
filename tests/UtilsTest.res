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
    Utils.positionFilterPred(list{{val: 2, pos: { x: 0, y: 0 } }}, (0, 0)),
    false
  )

  assertion(
    ~message = "In other cases let's keep it",
    ~operator = "positionFilterPred",
    (a, b) => a == b,
    Utils.positionFilterPred(list{{val: 2, pos: { x: 0, y: 0 } }}, (1, 1)),
    true
  )
})
