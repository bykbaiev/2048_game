@module external styles: Js.Dict.t<'a> = "./Board.module.css"

@val external document: 'a = "document"

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
  let (tiles, setTiles) = Recoil.useRecoilState(State.tilesState);

  React.useEffect0(() => {
    document["addEventListener"](."keydown", event => {
      switch Utils.keyCodeToDirection(event["keyCode"]) {
      | Some(dir) => setTiles(Utils.move(dir))
      | _ => ()
      }
    })
    Some(() => {
      document["removeEventListener"](."keydown")
    })
  })

  <div className={getClassName("root")}>
    <div className={getClassName("gridContainer")}>
      {viewRow -> viewGridSizedList}
    </div>
    <div className={getClassName("tileContainer")}>
      {tiles -> Belt.List.toArray -> viewList(viewTile)}
    </div>
  </div>
}
