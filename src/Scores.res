@module external styles: Js.Dict.t<'a> = "./Scores.module.css"

let getClassName = Utils.getCls(styles)

let prepareCounter = x => x -> Belt.Int.toString -> React.string

@react.component
let make = () => {
  let (prevScore, setPrevScore) = React.useState(_ => None)
  let (diff, setDiff)           = React.useState(_ => None)
  let (timeoutId, setTimeoutId) = React.useState(_ => None)

  let score = Recoil.useRecoilValue(State.scoreState)
  let best  = Recoil.useRecoilValue(State.bestScoreState)

  React.useEffect1(() => {
    switch timeoutId {
    | Some(id) => Js.Global.clearTimeout(id)
    | None     => ()
    }

    setDiff(_ => {
      switch prevScore {
      | Some(value) => score - value > 0
        ? {
          setTimeoutId(_ => Some(Js.Global.setTimeout(() => {
            setDiff(_ => None)
          }, 600)))
          Some(score - value)
        }
        : None
      | None        => None
      }
    })
    setPrevScore(_ => Some(score))
    None
  }, [score])

  <div className={getClassName("root")}>
    <div className={getClassName("score")}>
      {prepareCounter(score)}
      {Belt.Option.isSome(diff)
        ? (
          <div className={getClassName("scoreAddition")}>
            {diff -> Belt.Option.getWithDefault(0) -> Belt.Int.toString -> React.string}
          </div>
        )
        : React.null}
    </div>
    <div className={getClassName("best")}>
      {prepareCounter(best)}
    </div>
  </div>
}
