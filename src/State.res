open Tile

type stateInternals = {
  best:  option<int>,
  score: int,
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
    best:  None,
    score: 0,
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

let setScore = (internals: stateInternals, score: int): stateInternals => {
  { ...internals, score: score }
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
    let internals = get(internalsState)

    internals.score
  }
})

let bestScoreState: Recoil.readOnly<int> = Recoil.selector({
  key: "bestScoreState",
  get: ({ get }) => {
    let internals = get(internalsState)
    let score = get(scoreState)

    Belt.Option.mapWithDefault(internals.best, score, x => x > score ? x : score)
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

let isPlayingAfterWin = (state: state): bool => {
  switch state {
  | PlayingAfterWin(_) => true
  | _                  => false
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
