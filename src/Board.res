@module external styles: Js.Dict.t<'a> = "./Board.module.css"

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

let viewTile = ({ val, pos }: Utils.tile) => {
  let valName = val > Utils.winningValue ? "tileSuper" : j`tile-$val`
  let { x, y } = pos
  let posName = `tilePosition-${Js.Int.toString(x + 1)}-${Js.Int.toString(y + 1)}`
  <div key={j`$val-$x-$y-tile`} className={`${getClassName("tile")} ${getClassName(valName)} ${getClassName(posName)}`}>
    <div className={getClassName("tileInner")}>
      {React.string(Js.Int.toString(val))}
    </div>
  </div>
}

@react.component
let make = () => {
  let (tiles, _) = Recoil.useRecoilState(State.tilesState);

  <div className={getClassName("root")}>
    <div className={getClassName("gridContainer")}>
      {viewRow -> viewGridSizedList}
    </div>
    <div className={getClassName("tileContainer")}>
      {tiles -> Belt.List.toArray -> viewList(viewTile)}
    </div>
  </div>
}
