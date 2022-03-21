@module external styles: Js.Dict.t<'a> = "./Header.module.css"

let getClassName = Utils.getCls(styles)

@react.component
let make = () => {
  <div className={getClassName("root")}>
    <h1 className={getClassName("title")}>
      {React.string("2048")}
    </h1>
    <Scores />
  </div>
}
