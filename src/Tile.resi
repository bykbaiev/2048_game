module GameTile: {
  type tile

  module Getters: {
    let x: tile => int

    let y: tile => int

    let id: tile => string

    let val: tile => int

    let new: tile => bool

    let merged: tile => bool
  }

  module Setters: {
    let id: (tile, string) => tile

    let x: (tile, int) => tile

    let y: (tile, int) => tile

    let val: (tile, int) => tile

    let new: (tile, bool) => tile

    let merged: (tile, bool) => tile
  }

  let createTile: (~id: string, ~val: int, ~x: int, ~y: int) => tile

  let createNewTile: list<tile> => tile

  let isWinningValue: tile => bool

  // ONLY FOR TESTING PURPOSES
  let getPair: (int, int) => option<(int, int)>

  let positionFilterPred: (list<tile>, (int, int)) => bool
}
