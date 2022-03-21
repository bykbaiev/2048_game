// Generated by ReScript, PLEASE EDIT WITH CARE

import * as Curry from "../node_modules/rescript/lib/es6/curry.js";
import * as State from "./State.mjs";
import * as Utils from "./Utils.mjs";
import * as React from "react";
import * as Recoil from "recoil";
import * as GameIntroModuleCss from "./GameIntro.module.css";

var styles = GameIntroModuleCss;

function getClassName(param) {
  return Utils.getCls(styles, param);
}

function prepareCounter(x) {
  return String(x);
}

function GameIntro(Props) {
  var setState = Recoil.useSetRecoilState(State.gameState);
  var tryAgain = function (param) {
    return Curry._1(setState, (function (param) {
                  return State.initialize(undefined);
                }));
  };
  return React.createElement("div", {
              className: Utils.getCls(styles, "root")
            }, React.createElement("div", undefined, React.createElement("h2", {
                      className: Utils.getCls(styles, "subtitle")
                    }, "Play 2048 game online"), React.createElement("div", {
                      className: Utils.getCls(styles, "about")
                    }, "Join the numbers and get to the 2048 tile!")), React.createElement("button", {
                  className: Utils.getCls(styles, "restartButton"),
                  onClick: tryAgain
                }, "New Game"));
}

var make = GameIntro;

export {
  styles ,
  getClassName ,
  prepareCounter ,
  make ,
  
}
/* styles Not a pure module */
