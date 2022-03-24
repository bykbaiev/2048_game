// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Test from "../node_modules/rescript-test/src/Test.mjs";
import * as Tile from "../src/Tile.mjs";
import * as Curry from "../node_modules/rescript/lib/es6/curry.js";
import * as Caml_obj from "../node_modules/rescript/lib/es6/caml_obj.js";
import * as Caml_array from "../node_modules/rescript/lib/es6/caml_array.js";
import * as Caml_option from "../node_modules/rescript/lib/es6/caml_option.js";

function createTestTile(param) {
  var pos = param.pos;
  var average = Curry._1(Tile.GameTile.Converters.toAverage, Tile.GameTile.createTile(param.id, param.val, pos.x, pos.y));
  if (param.merged) {
    return Curry._1(Tile.GameTile.Converters.toMerged, average);
  } else {
    return average;
  }
}

function checkPairs(max, idx, expected) {
  return Test.assertion("check pairs for equality: " + max.toString() + " " + idx.toString(), "getPair", Caml_obj.caml_equal, Tile.GameTile.getPair(max, idx), expected);
}

var pairs2 = [
  [
    0,
    0
  ],
  [
    0,
    1
  ],
  [
    1,
    0
  ],
  [
    1,
    1
  ]
];

var pairs4 = [
  [
    0,
    0
  ],
  [
    0,
    1
  ],
  [
    0,
    2
  ],
  [
    0,
    3
  ],
  [
    1,
    0
  ],
  [
    1,
    1
  ],
  [
    1,
    2
  ],
  [
    1,
    3
  ],
  [
    2,
    0
  ],
  [
    2,
    1
  ],
  [
    2,
    2
  ],
  [
    2,
    3
  ],
  [
    3,
    0
  ],
  [
    3,
    1
  ],
  [
    3,
    2
  ],
  [
    3,
    3
  ]
];

Test.test("#Tile.getPair: should return correct pair from cortesian product by max value and index of this pair", (function (param) {
        checkPairs(2, 0, Caml_array.get(pairs2, 0));
        checkPairs(2, 1, Caml_array.get(pairs2, 1));
        checkPairs(2, 2, Caml_array.get(pairs2, 2));
        checkPairs(2, 3, Caml_array.get(pairs2, 3));
        checkPairs(4, 0, Caml_array.get(pairs4, 0));
        checkPairs(4, 1, Caml_array.get(pairs4, 1));
        checkPairs(4, 2, Caml_array.get(pairs4, 2));
        checkPairs(4, 3, Caml_array.get(pairs4, 3));
        checkPairs(4, 4, Caml_array.get(pairs4, 4));
        checkPairs(4, 5, Caml_array.get(pairs4, 5));
        checkPairs(4, 6, Caml_array.get(pairs4, 6));
        checkPairs(4, 7, Caml_array.get(pairs4, 7));
        checkPairs(4, 8, Caml_array.get(pairs4, 8));
        checkPairs(4, 9, Caml_array.get(pairs4, 9));
        checkPairs(4, 10, Caml_array.get(pairs4, 10));
        return checkPairs(4, 15, Caml_array.get(pairs4, 15));
      }));

Test.test("#Tile.getPair: should return Nothing in case there is no such pair", (function (param) {
        return checkPairs(4, 20, undefined);
      }));

Test.test("#Tile.positionFilterPred: should verify that pair is valid & there is no such tile", (function (param) {
        Test.assertion("Nothing won't come through me!", "positionFilterPred", (function (a, b) {
                return a === b;
              }), Tile.GameTile.positionFilterPred(/* [] */0, [
                  -1,
                  -1
                ]), false);
        Test.assertion("Existing tile won't come through me!", "positionFilterPred", (function (a, b) {
                return a === b;
              }), Tile.GameTile.positionFilterPred({
                  hd: createTestTile({
                        id: "0",
                        merged: false,
                        val: 2,
                        pos: {
                          x: 0,
                          y: 0
                        }
                      }),
                  tl: /* [] */0
                }, [
                  0,
                  0
                ]), false);
        return Test.assertion("In other cases let's keep it", "positionFilterPred", (function (a, b) {
                      return a === b;
                    }), Tile.GameTile.positionFilterPred({
                        hd: createTestTile({
                              id: "0",
                              merged: false,
                              val: 2,
                              pos: {
                                x: 0,
                                y: 0
                              }
                            }),
                        tl: /* [] */0
                      }, [
                        1,
                        1
                      ]), true);
      }));

Test.test("#Tile.encode: should encode the tile to keep the history", (function (param) {
        var dict = {};
        Test.assertion("Average tile", "encode", Caml_obj.caml_equal, Tile.GameTile.encode(createTestTile({
                      id: "0",
                      merged: false,
                      val: 2,
                      pos: {
                        x: 0,
                        y: 1
                      }
                    })), (dict["status"] = "average", dict["id"] = "0", dict["val"] = 2, dict["x"] = 0, dict["y"] = 1, dict));
        var dict$1 = {};
        Test.assertion("Merged tile", "encode", Caml_obj.caml_equal, Tile.GameTile.encode(createTestTile({
                      id: "0",
                      merged: true,
                      val: 2,
                      pos: {
                        x: 0,
                        y: 1
                      }
                    })), (dict$1["status"] = "merged", dict$1["id"] = "0", dict$1["val"] = 2, dict$1["x"] = 0, dict$1["y"] = 1, dict$1));
        var dict$2 = {};
        return Test.assertion("New tile", "encode", Caml_obj.caml_equal, Tile.GameTile.encode(Curry._1(Tile.GameTile.Converters.toNew, createTestTile({
                                id: "0",
                                merged: false,
                                val: 2,
                                pos: {
                                  x: 0,
                                  y: 1
                                }
                              }))), (dict$2["status"] = "new", dict$2["id"] = "0", dict$2["val"] = 2, dict$2["x"] = 0, dict$2["y"] = 1, dict$2));
      }));

Test.test("#Tile.decode: should decode the tile JSON representation to actual tile", (function (param) {
        Test.assertion("Invalid tile", "decode", Caml_obj.caml_equal, Tile.GameTile.decode(JSON.parse("{}")), undefined);
        Test.assertion("Invalid tile (2)", "decode", Caml_obj.caml_equal, Tile.GameTile.decode(JSON.parse("{\"status\": \"non-existing\"}")), undefined);
        Test.assertion("Invalid tile (3)", "decode", Caml_obj.caml_equal, Tile.GameTile.decode(JSON.parse("{\"status\": \"merged\"}")), undefined);
        Test.assertion("Average tile", "decode", Caml_obj.caml_equal, Tile.GameTile.decode(JSON.parse("{\"status\": \"average\", \"id\": \"tile-123\", \"val\": 4, \"x\": 0, \"y\": 1 }")), Caml_option.some(createTestTile({
                      id: "tile-123",
                      merged: false,
                      val: 4,
                      pos: {
                        x: 0,
                        y: 1
                      }
                    })));
        Test.assertion("Merged tile", "decode", Caml_obj.caml_equal, Tile.GameTile.decode(JSON.parse("{\"status\": \"merged\", \"id\": \"tile-123\", \"val\": 4, \"x\": 0, \"y\": 1 }")), Caml_option.some(createTestTile({
                      id: "tile-123",
                      merged: true,
                      val: 4,
                      pos: {
                        x: 0,
                        y: 1
                      }
                    })));
        return Test.assertion("New tile", "decode", Caml_obj.caml_equal, Tile.GameTile.decode(JSON.parse("{\"status\": \"new\", \"id\": \"tile-123\", \"val\": 4, \"x\": 0, \"y\": 1 }")), Caml_option.some(Curry._1(Tile.GameTile.Converters.toNew, createTestTile({
                                id: "tile-123",
                                merged: false,
                                val: 4,
                                pos: {
                                  x: 0,
                                  y: 1
                                }
                              }))));
      }));

Test.test("#Tile.encodeHistorical: dumb tile representation for history", (function (param) {
        Test.assertion("New tile", "decode", Caml_obj.caml_equal, Tile.GameTile.encodeHistorical(createTestTile({
                      id: "tile-1",
                      merged: false,
                      val: 2,
                      pos: {
                        x: 0,
                        y: 1
                      }
                    })), [
              "tile-1",
              "2",
              "0",
              "1"
            ]);
        Test.assertion("Merged tile", "decode", Caml_obj.caml_equal, Tile.GameTile.encodeHistorical(createTestTile({
                      id: "tile-1",
                      merged: true,
                      val: 2,
                      pos: {
                        x: 0,
                        y: 1
                      }
                    })), [
              "tile-1",
              "2",
              "0",
              "1"
            ]);
        return Test.assertion("Average tile", "decode", Caml_obj.caml_equal, Tile.GameTile.encodeHistorical(Curry._1(Tile.GameTile.Converters.toAverage, createTestTile({
                                id: "tile-1",
                                merged: false,
                                val: 2,
                                pos: {
                                  x: 0,
                                  y: 1
                                }
                              }))), [
                    "tile-1",
                    "2",
                    "0",
                    "1"
                  ]);
      }));

Test.test("#Tile.decodeHistorical: tile from it's dumb representation", (function (param) {
        Test.assertion("Invalid tile", "decodeHistorical", Caml_obj.caml_equal, Tile.GameTile.decodeHistorical(JSON.parse("[]")), undefined);
        Test.assertion("Invalid tile (2)", "decodeHistorical", Caml_obj.caml_equal, Tile.GameTile.decodeHistorical(JSON.parse("[0, 2, \"asdf\", 3]")), undefined);
        Test.assertion("Invalid tile (3)", "decodeHistorical", Caml_obj.caml_equal, Tile.GameTile.decodeHistorical(JSON.parse("[\"0\", 2, 3]")), undefined);
        return Test.assertion("Valid tile", "decodeHistorical", Caml_obj.caml_equal, Tile.GameTile.decodeHistorical(JSON.parse("[\"tile-123\", 2, 3, 3]")), Caml_option.some(Curry._1(Tile.GameTile.Converters.toAverage, createTestTile({
                                id: "tile-123",
                                merged: false,
                                val: 2,
                                pos: {
                                  x: 3,
                                  y: 3
                                }
                              }))));
      }));

export {
  createTestTile ,
  checkPairs ,
  pairs2 ,
  pairs4 ,
  
}
/*  Not a pure module */
