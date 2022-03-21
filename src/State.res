open Tile

type stateInternals = {
  best: option<int>,
  tiles: list<GameTile.tile>
}

type state =
| Playing(stateInternals)
| Win(stateInternals)
| Loss(stateInternals)
| PlayingAfterWin(stateInternals)

// SIDE EFFECT
let initialize = () => {
  let fst = GameTile.createNewTile(list{})
  let snd = GameTile.createNewTile(list{ fst })
  Playing({
    best: None,
    tiles: list{ fst, snd }
  })
}

let gameState: Recoil.readWrite<state> = Recoil.atom({
  key: "gameState",
  default: {
    initialize()
  }
})

let getInternals = (state: state): stateInternals => {
  switch state {
  | Playing(internals)
  | Win(internals)
  | Loss(internals)
  | PlayingAfterWin(internals) => internals
  }
}

let internalsState: Recoil.readOnly<stateInternals> = Recoil.selector({
  key: "internalsState",
  get: ({ get }) => {
    let state = get(gameState)

    getInternals(state)
  }
})

let setTiles = (internals: stateInternals, tiles: list<GameTile.tile>): stateInternals => {
  { ...internals, tiles: tiles }
}

let tilesState: Recoil.readOnly<list<GameTile.tile>> = Recoil.selector({
  key: "tilesState",
  get: ({ get }) => {
    let internals = get(internalsState)

    internals.tiles
  }
})

let scoreState: Recoil.readOnly<int> = Recoil.selector({
  key: "scoreState",
  get: ({ get }) => {
    let tiles = get(tilesState)
    tiles
    -> Belt.List.map(GameTile.Getters.val)
    -> Belt.List.sort((a, b) => b - a)
    -> Belt.List.head
    -> Belt.Option.getWithDefault(0)
  }
})

let bestScoreState: Recoil.readOnly<int> = Recoil.selector({
  key: "bestScoreState",
  get: ({ get }) => {
    let internals = get(internalsState)
    let score = get(scoreState)

    Belt.Option.getWithDefault(internals.best, score)
  }
})

let isWin = (state: state): bool => {
  switch state {
  | Win(_) => true
  | _      => false
  }
}

let isLoss = (state: state): bool => {
  switch state {
  | Loss(_) => true
  | _       => false
  }
}

let winState: Recoil.readOnly<bool> = Recoil.selector({
  key: "winState",
  get: ({ get }) => {
    let state = get(gameState)

    isWin(state)
  }
})

let lossState: Recoil.readOnly<bool> = Recoil.selector({
  key: "lossState",
  get: ({ get }) => {
    let state = get(gameState)

    isLoss(state)
  }
})

let endOfGameState: Recoil.readOnly<bool> = Recoil.selector({
  key: "endOfGameState",
  get: ({ get }) => {
    get(winState) || get(lossState)
  }
})

let messageState: Recoil.readOnly<option<string>> = Recoil.selector({
  key: "messageState",
  get: ({ get }) => {
    let state = get(gameState)

    switch state {
    | Win(_)  => Some("You win!")
    | Loss(_) => Some("Game over!")
    | _       => None
    }
  }
})
