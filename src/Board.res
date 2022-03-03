@module external styles: Js.Dict.t<'a> = "./Board.module.css"

let gridSize = 4

let winningValue = 2048

let indexes = Belt.Array.makeBy(gridSize, idx => idx)

type position = {
  x: int,
  y: int
}

type tile = {
  val: int,
  pos: position
}

let fstTile = { val: 2, pos: { x: 2, y: 2 }}

let tiles = list{
  { val: 2,    pos: { x: 2, y: 2 } },
  { val: 1024, pos: { x: 0, y: 3 } }
}

let getClassName = (name: string): string =>
  styles
  -> Js.Dict.get("default")
  -> Belt.Option.flatMap(defaults => Js.Dict.get(defaults, name))
  -> Belt.Option.getWithDefault("")

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

let viewTile = ({ val, pos }: tile) => {
  let valName = val > winningValue ? "tileSuper" : j`tile-$val`
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
  <div className={getClassName("root")}>
    <div className={getClassName("gridContainer")}>
      {viewRow -> viewGridSizedList}
    </div>
    <div className={getClassName("tileContainer")}>
      {tiles -> Belt.List.toArray -> viewList(viewTile)}
    </div>
  </div>
}
