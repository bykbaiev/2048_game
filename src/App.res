%%raw("import './reset.css'")
%%raw("import './styles.css'")

@module external styles: Js.Dict.t<'a> = "./App.module.css"

let getClassName = Utils.getCls(styles)

@react.component
let make = () => {
  <Recoil.RecoilRoot>
    <div className={getClassName("root")}>
      <Header />
      <GameIntro />
      <Board />
    </div>
  </Recoil.RecoilRoot>
}
