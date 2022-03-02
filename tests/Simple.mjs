// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "../node_modules/rescript-test/src/Test.mjs";

function intEqual(message, a, b) {
  return Test.assertion(message, "intEqual", (function (a, b) {
                return a === b;
              }), a, b);
}

function stringEqual(message, a, b) {
  return Test.assertion(message, "stringEqual", (function (a, b) {
                return a === b;
              }), a, b);
}

Test.test("Equals", (function (param) {
        return intEqual(undefined, 1, 1);
      }));

function userEq(a, b) {
  return a.id === b.id;
}

function userEqual(message, a, b) {
  return Test.assertion(message, "userEqual", userEq, a, b);
}

Test.test("DeepEquals", (function (param) {
        stringEqual(undefined, "user", "user");
        return userEqual(undefined, {
                    username: "user",
                    id: "a"
                  }, {
                    username: "user",
                    id: "a"
                  });
      }));

export {
  intEqual ,
  stringEqual ,
  userEq ,
  userEqual ,
  
}
/*  Not a pure module */