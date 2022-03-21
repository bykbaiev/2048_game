@module external styles: Js.Dict.t<'a> = "./GameIntro.module.css"

let getClassName = Utils.getCls(styles)

let prepareCounter = x => x -> Belt.Int.toString -> React.string

@react.component
let make = () => {
  let setState = Recoil.useSetRecoilState(State.gameState)

  let tryAgain = (_) => {
    setState(_ => State.initialize())
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
      className={getClassName("restartButton")}
      onClick={tryAgain}
    >
      {React.string("New Game")}
    </button>
  </div>
}
