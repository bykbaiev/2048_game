// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Tile from "./Tile.mjs";
import * as Curry from "../node_modules/rescript/lib/es6/curry.js";
import * as Recoil from "recoil";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Belt_Option from "../node_modules/rescript/lib/es6/belt_Option.js";

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

var gameState = Recoil.atom({
      key: "gameState",
      default: initialize(undefined)
    });

function getInternals(state) {
  return state._0;
}

var internalsState = Recoil.selector({
      key: "internalsState",
      get: (function (param) {
          return Curry._1(param.get, gameState)._0;
        })
    });

function setTiles(internals, tiles) {
  return {
          best: internals.best,
          tiles: tiles
        };
}

var tilesState = Recoil.selector({
      key: "tilesState",
      get: (function (param) {
          return Curry._1(param.get, internalsState).tiles;
        })
    });

var scoreState = Recoil.selector({
      key: "scoreState",
      get: (function (param) {
          var tiles = Curry._1(param.get, tilesState);
          return Belt_Option.getWithDefault(Belt_List.head(Belt_List.sort(Belt_List.map(tiles, Tile.GameTile.Getters.val), (function (a, b) {
                                return b - a | 0;
                              }))), 0);
        })
    });

var bestScoreState = Recoil.selector({
      key: "bestScoreState",
      get: (function (param) {
          var get = param.get;
          var internals = Curry._1(get, internalsState);
          var score = Curry._1(get, scoreState);
          return Belt_Option.getWithDefault(internals.best, score);
        })
    });

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
  initialize ,
  gameState ,
  getInternals ,
  internalsState ,
  setTiles ,
  tilesState ,
  scoreState ,
  bestScoreState ,
  isWin ,
  isLoss ,
  winState ,
  lossState ,
  endOfGameState ,
  messageState ,
  
}
/* gameState Not a pure module */
