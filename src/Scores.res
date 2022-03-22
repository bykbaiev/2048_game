@module external styles: Js.Dict.t<'a> = "./Scores.module.css"

let getClassName = Utils.getCls(styles)

let prepareCounter = x => x -> Belt.Int.toString -> React.string

@react.component
let make = () => {
  let score = Recoil.useRecoilValue(State.scoreState)
  let best = Recoil.useRecoilValue(State.bestScoreState)

  <div className={getClassName("root")}>
    <div className={getClassName("score")}>
      {prepareCounter(score)}
    </div>
    <div className={getClassName("best")}>
      {prepareCounter(best)}
    </div>
  </div>
}
