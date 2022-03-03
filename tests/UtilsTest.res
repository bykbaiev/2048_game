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
