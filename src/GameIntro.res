@module external styles: Js.Dict.t<'a> = "./GameIntro.module.css"

let getClassName = Utils.getCls(styles)

@react.component
let make = () => {
  let setState              = Recoil.useSetRecoilState(State.gameState)
  let (history, setHistory) = Recoil.useRecoilState(State.historyState)

  let tryAgain = (_) => {
    setState(_ => State.initialize())
    setHistory(_ => [])
  }

  let undo = (_) => {
    let previous = Belt.Array.get(history, Belt.Array.length(history) - 2)
    switch previous {
    | Some(value) => setState(State.rollbackState(value))
    | None        => ()
    }
    setHistory(Utils.removeLastTwo)
  }

  <div className={getClassName("root")}>
    <div>
      <h2 className={getClassName("subtitle")}>
        {React.string("Play 2048 game online")}
      </h2>
      <div className={getClassName("about")}>
        {React.string("Join the numbers and get to the 2048 tile!")}
      </div>
    </div>
    <button
      className={`${getClassName("btn")} ${getClassName("btnUndo")}`}
      onClick={undo}
    >
      {React.string("Undo")}
    </button>
    <button
      className={getClassName("btn")}
      onClick={tryAgain}
    >
      {React.string("New Game")}
    </button>
  </div>
}
