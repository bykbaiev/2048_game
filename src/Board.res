@module external styles: {..} = "./Board.module.css"

let gridSize = 4

let indexes = Belt.Array.makeBy(gridSize, idx => idx)

let viewList = render => indexes -> Belt.Array.map(render) -> React.array

let viewCell = (rowId, cellId) => {
  <div key={j`$rowId-$cellId-cell`} className={styles["gridCell"]} />
}

let viewRow = (rowId) => {
  <div key={j`$rowId-row`} className={styles["gridRow"]}>
    {rowId -> viewCell -> viewList}
  </div>
}

@react.component
let make = () => {
  <div className={styles["root"]}>
    <div className={styles["grid-container"]}>
      {viewRow -> viewList}
    </div>
  </div>
}
