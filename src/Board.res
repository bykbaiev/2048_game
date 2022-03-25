@module external styles: Js.Dict.t<'a> = "./Board.module.css"

@val external document: Dom.document = "document"

@send
external addEventListener: (Dom.document, string, {..} => unit) => unit = "addEventListener"

@send
external removeEventListener: (Dom.document, string) => unit = "removeEventListener"

open Tile

let indexes = Belt.Array.makeBy(Utils.gridSize, idx => idx)

let getClassName = Utils.getCls(styles)

let viewList = (items, render) => items -> Belt.Array.map(render) -> React.array

let viewGridSizedList = viewList(indexes)

let viewCell = (rowId, cellId) => {
  <div key={j`$rowId-$cellId-cell`} className={getClassName("gridCell")} />
}

let viewRow = (rowId) => {
  <div key={j`$rowId-row`} className={getClassName("gridRow")}>
    {rowId -> viewCell -> viewGridSizedList}
  </div>
}

let viewTile = (tile: GameTile.tile) => {
  let val = GameTile.Getters.val(tile)
  let valName = val > Constants.winningValue ? "tileSuper" : j`tile-$val`
  let x = GameTile.Getters.x(tile)
  let y = GameTile.Getters.y(tile)
  let posName = `tilePosition-${Js.Int.toString(x + 1)}-${Js.Int.toString(y + 1)}`
  let newName = GameTile.Getters.new(tile) ? "tileNew" : ""
  let mergedName = GameTile.Getters.merged(tile) ? "tileMerged" : ""
  <div key={GameTile.Getters.id(tile)} className={`${getClassName("tile")} ${getClassName(valName)} ${getClassName(posName)} ${getClassName(newName)} ${getClassName(mergedName)}`}>
    <div className={getClassName("tileInner")}>
      {React.string(Js.Int.toString(val))}
    </div>
  </div>
}

@react.component
let make = () => {
  let setState   = Recoil.useSetRecoilState(State.gameState)
  let tiles      = Recoil.useRecoilValue(State.tilesState)
  let isWin      = Recoil.useRecoilValue(State.winState)
  let isLoss     = Recoil.useRecoilValue(State.lossState)
  let message    = Recoil.useRecoilValue(State.messageState)

  React.useEffect0(() => {
    addEventListener(document, "keydown", event => {
      switch Utils.keyCodeToDirection(event["keyCode"]) {
      | Some(dir) => setState(Utils.move(dir))
      | _ => ()
      }
    })
    Some(() => {
      removeEventListener(document, "keydown")
    })
  })

  let continueGame = (_) => {
    setState(state => state -> State.getInternals -> State.PlayingAfterWin)
  }

  let tryAgain = (_) => {
    setState(_ => State.initialize())
  }

  <div className={getClassName("root")}>
    {(isWin || isLoss) ? (
      <div className={`${getClassName("gameMessage")} ${isWin ? getClassName("gameWon") : ""}`}>
        <p>{React.string(Belt.Option.getWithDefault(message, ""))}</p>
        <div className={getClassName("lower")}>
          {isWin ? (
            <button
              className={getClassName("gameMessageButton")}
              onClick={continueGame}
            >
              {React.string("Keep going")}
            </button>
          ) : React.null}
          <button
            className={getClassName("gameMessageButton")}
            onClick={tryAgain}
          >
            {React.string("Try again")}
          </button>
        </div>
      </div>
    ) : React.null}
    <div className={getClassName("gridContainer")}>
      {viewRow -> viewGridSizedList}
    </div>
    <div className={getClassName("tileContainer")}>
      {tiles -> Belt.List.toArray -> viewList(viewTile)}
    </div>
  </div>
}
