// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Tile from "./Tile.mjs";
import * as Curry from "../node_modules/rescript/lib/es6/curry.js";
import * as Recoil from "recoil";
import * as Js_dict from "../node_modules/rescript/lib/es6/js_dict.js";
import * as Js_json from "../node_modules/rescript/lib/es6/js_json.js";
import * as Js_math from "../node_modules/rescript/lib/es6/js_math.js";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Option from "../node_modules/rescript/lib/es6/belt_Option.js";
import * as Dom_storage from "../node_modules/rescript/lib/es6/dom_storage.js";

var gameStateKey = "gameState";

var bestScoreKey = "bestScore";

function initialize(param) {
  var fst = Tile.GameTile.createNewTile(/* [] */0);
  var snd = Tile.GameTile.createNewTile({
        hd: fst,
        tl: /* [] */0
      });
  return {
          TAG: /* Playing */0,
          _0: {
            best: undefined,
            score: 0,
            tiles: {
              hd: fst,
              tl: {
                hd: snd,
                tl: /* [] */0
              }
            }
          }
        };
}

function getInternals(state) {
  return state._0;
}

function getBestScore(state) {
  var internals = state._0;
  var best = internals.best;
  var score = internals.score;
  return Belt_Option.mapWithDefault(best, score, (function (param) {
                return Math.max(score, param);
              }));
}

function isWin(state) {
  if (state.TAG === /* Win */1) {
    return true;
  } else {
    return false;
  }
}

function isLoss(state) {
  if (state.TAG === /* Loss */2) {
    return true;
  } else {
    return false;
  }
}

function isPlayingAfterWin(state) {
  if (state.TAG === /* PlayingAfterWin */3) {
    return true;
  } else {
    return false;
  }
}

function setTiles(internals, tiles) {
  return {
          best: internals.best,
          score: internals.score,
          tiles: tiles
        };
}

function setScore(internals, score) {
  return {
          best: internals.best,
          score: score,
          tiles: internals.tiles
        };
}

function setBestScore(internals, best) {
  return {
          best: best,
          score: internals.score,
          tiles: internals.tiles
        };
}

function encodeBestScore(prim) {
  return prim;
}

function encodeStatus(state) {
  switch (state.TAG | 0) {
    case /* Playing */0 :
        return "playing";
    case /* Win */1 :
        return "win";
    case /* Loss */2 :
        return "loss";
    case /* PlayingAfterWin */3 :
        return "playingAfterWin";
    
  }
}

function encodeGameState(state) {
  var status = encodeStatus(state);
  var match = state._0;
  var gameState = {};
  gameState["status"] = status;
  gameState["score"] = match.score;
  gameState["tiles"] = Belt_List.toArray(Belt_List.map(match.tiles, Tile.GameTile.encode));
  return gameState;
}

function decodeBestScore(best) {
  if (best === undefined) {
    return ;
  }
  var scoreJson;
  try {
    scoreJson = JSON.parse(best);
  }
  catch (exn){
    scoreJson = "invalid";
  }
  var value = Js_json.classify(scoreJson);
  if (typeof value === "number" || value.TAG !== /* JSONNumber */1) {
    return ;
  } else {
    return Js_math.ceil_int(value._0);
  }
}

function decodeGameState(state) {
  if (state === undefined) {
    return ;
  }
  var json;
  try {
    json = JSON.parse(state);
  }
  catch (exn){
    json = 0;
  }
  var value = Js_json.classify(json);
  if (typeof value === "number") {
    return ;
  }
  if (value.TAG !== /* JSONObject */2) {
    return ;
  }
  var value$1 = value._0;
  var status = Belt_Option.flatMap(Js_dict.get(value$1, "status"), Js_json.decodeString);
  var score = Belt_Option.map(Belt_Option.flatMap(Js_dict.get(value$1, "score"), Js_json.decodeNumber), Js_math.ceil_int);
  var tiles = Belt_Option.map(Belt_Option.flatMap(Js_dict.get(value$1, "tiles"), Js_json.decodeArray), (function (ts) {
          return Belt_List.keep(Belt_List.map(Belt_List.fromArray(ts), Tile.GameTile.decode), Belt_Option.isSome);
        }));
  var internals = score !== undefined && tiles !== undefined ? ({
        best: undefined,
        score: score,
        tiles: Belt_List.map(tiles, (function (tile) {
                return Belt_Option.getWithDefault(tile, Tile.GameTile.createNewTile(/* [] */0));
              }))
      }) : undefined;
  if (status === undefined) {
    return ;
  }
  switch (status) {
    case "loss" :
        if (internals !== undefined) {
          return {
                  TAG: /* Loss */2,
                  _0: internals
                };
        } else {
          return ;
        }
    case "playing" :
        if (internals !== undefined) {
          return {
                  TAG: /* Playing */0,
                  _0: internals
                };
        } else {
          return ;
        }
    case "playingAfterWin" :
        if (internals !== undefined) {
          return {
                  TAG: /* PlayingAfterWin */3,
                  _0: internals
                };
        } else {
          return ;
        }
    case "win" :
        if (internals !== undefined) {
          return {
                  TAG: /* Win */1,
                  _0: internals
                };
        } else {
          return ;
        }
    default:
      return ;
  }
}

function localStorageEffect(param) {
  var savedGameState = Dom_storage.getItem(gameStateKey, localStorage);
  var savedBestScore = Dom_storage.getItem(bestScoreKey, localStorage);
  Curry._1(param.onSet, (function (newValue, param, isReset) {
          if (isReset) {
            return Dom_storage.removeItem(bestScoreKey, localStorage);
          } else {
            Dom_storage.setItem(bestScoreKey, JSON.stringify(getBestScore(newValue)), localStorage);
            return Dom_storage.setItem(gameStateKey, JSON.stringify(encodeGameState(newValue)), localStorage);
          }
        }));
  var bestScore = decodeBestScore(savedBestScore);
  var gameState = decodeGameState(savedGameState);
  Curry._1(param.setSelf, (function (state) {
          var actualState = Belt_Option.getWithDefault(gameState, state);
          var internals = actualState._0;
          var updated = setBestScore(internals, bestScore);
          switch (actualState.TAG | 0) {
            case /* Playing */0 :
                return {
                        TAG: /* Playing */0,
                        _0: updated
                      };
            case /* Win */1 :
                return {
                        TAG: /* Win */1,
                        _0: updated
                      };
            case /* Loss */2 :
                return {
                        TAG: /* Loss */2,
                        _0: updated
                      };
            case /* PlayingAfterWin */3 :
                return {
                        TAG: /* PlayingAfterWin */3,
                        _0: updated
                      };
            
          }
        }));
  
}

var gameState = Recoil.atom({
      key: "gameState",
      default: initialize(undefined),
      effects_UNSTABLE: [localStorageEffect]
    });

var internalsState = Recoil.selector({
      key: "internalsState",
      get: (function (param) {
          return Curry._1(param.get, gameState)._0;
        })
    });

var tilesState = Recoil.selector({
      key: "tilesState",
      get: (function (param) {
          return Curry._1(param.get, internalsState).tiles;
        })
    });

var scoreState = Recoil.selector({
      key: "scoreState",
      get: (function (param) {
          return Curry._1(param.get, internalsState).score;
        })
    });

var bestScoreState = Recoil.selector({
      key: "bestScoreState",
      get: (function (param) {
          return getBestScore(Curry._1(param.get, gameState));
        })
    });

var winState = Recoil.selector({
      key: "winState",
      get: (function (param) {
          return isWin(Curry._1(param.get, gameState));
        })
    });

var lossState = Recoil.selector({
      key: "lossState",
      get: (function (param) {
          return isLoss(Curry._1(param.get, gameState));
        })
    });

var endOfGameState = Recoil.selector({
      key: "endOfGameState",
      get: (function (param) {
          var get = param.get;
          if (Curry._1(get, winState)) {
            return true;
          } else {
            return Curry._1(get, lossState);
          }
        })
    });

var messageState = Recoil.selector({
      key: "messageState",
      get: (function (param) {
          var state = Curry._1(param.get, gameState);
          switch (state.TAG | 0) {
            case /* Win */1 :
                return "You win!";
            case /* Loss */2 :
                return "Game over!";
            case /* Playing */0 :
            case /* PlayingAfterWin */3 :
                return ;
            
          }
        })
    });

export {
  gameStateKey ,
  bestScoreKey ,
  initialize ,
  getInternals ,
  getBestScore ,
  isWin ,
  isLoss ,
  isPlayingAfterWin ,
  setTiles ,
  setScore ,
  setBestScore ,
  encodeBestScore ,
  encodeStatus ,
  encodeGameState ,
  decodeBestScore ,
  decodeGameState ,
  localStorageEffect ,
  gameState ,
  internalsState ,
  tilesState ,
  scoreState ,
  bestScoreState ,
  winState ,
  lossState ,
  endOfGameState ,
  messageState ,
  
}
/* gameState Not a pure module */
