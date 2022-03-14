// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Tile from "./Tile.mjs";
import * as Curry from "../node_modules/rescript/lib/es6/curry.js";
import * as State from "./State.mjs";
import * as Utils from "./Utils.mjs";
import * as React from "react";
import * as Recoil from "recoil";
import * as Belt_List from "../node_modules/rescript/lib/es6/belt_List.js";
import * as Constants from "./Constants.mjs";
import * as Belt_Array from "../node_modules/rescript/lib/es6/belt_Array.js";
import * as BoardModuleCss from "./Board.module.css";

var styles = BoardModuleCss;

var indexes = Belt_Array.makeBy(Utils.gridSize, (function (idx) {
        return idx;
      }));

function getClassName(param) {
  return Utils.getCls(styles, param);
}

var viewList = Belt_Array.map;

function viewGridSizedList(param) {
  return Belt_Array.map(indexes, param);
}

function viewCell(rowId, cellId) {
  return React.createElement("div", {
              key: "" + rowId + "-" + cellId + "-cell",
              className: Utils.getCls(styles, "gridCell")
            });
}

function viewRow(rowId) {
  return React.createElement("div", {
              key: "" + rowId + "-row",
              className: Utils.getCls(styles, "gridRow")
            }, Belt_Array.map(indexes, (function (param) {
                    return viewCell(rowId, param);
                  })));
}

function viewTile(tile) {
  var val = Curry._1(Tile.GameTile.Getters.val, tile);
  var valName = val > Constants.winningValue ? "tileSuper" : "tile-" + val;
  var x = Curry._1(Tile.GameTile.Getters.x, tile);
  var y = Curry._1(Tile.GameTile.Getters.y, tile);
  var posName = "tilePosition-" + (x + 1 | 0).toString() + "-" + (y + 1 | 0).toString();
  var newName = Curry._1(Tile.GameTile.Getters.$$new, tile) ? "tileNew" : "";
  var mergedName = Curry._1(Tile.GameTile.Getters.merged, tile) ? "tileMerged" : "";
  return React.createElement("div", {
              key: Curry._1(Tile.GameTile.Getters.id, tile),
              className: Utils.getCls(styles, "tile") + " " + Utils.getCls(styles, valName) + " " + Utils.getCls(styles, posName) + " " + Utils.getCls(styles, newName) + " " + Utils.getCls(styles, mergedName)
            }, React.createElement("div", {
                  className: Utils.getCls(styles, "tileInner")
                }, val.toString()));
}

function Board(Props) {
  var match = Recoil.useRecoilState(State.tilesState);
  var setTiles = match[1];
  React.useEffect((function () {
          document.addEventListener("keydown", (function ($$event) {
                  var dir = Utils.keyCodeToDirection($$event.keyCode);
                  if (dir !== undefined) {
                    return Curry._1(setTiles, (function (param) {
                                  return Utils.move(dir, param);
                                }));
                  }
                  
                }));
          return (function (param) {
                    return document.removeEventListener("keydown");
                  });
        }), []);
  return React.createElement("div", {
              className: Utils.getCls(styles, "root")
            }, React.createElement("div", {
                  className: Utils.getCls(styles, "gridContainer")
                }, Belt_Array.map(indexes, viewRow)), React.createElement("div", {
                  className: Utils.getCls(styles, "tileContainer")
                }, Belt_Array.map(Belt_List.toArray(match[0]), viewTile)));
}

var make = Board;

export {
  styles ,
  indexes ,
  getClassName ,
  viewList ,
  viewGridSizedList ,
  viewCell ,
  viewRow ,
  viewTile ,
  make ,
  
}
/* styles Not a pure module */
