%%raw("import './reset.css'")

@module external styles: {..} = "./App.module.css"

let element = <div> {React.string("Hello World")} </div>

let heading = <h1> {React.string("Overview 123")} </h1>

@react.component
let make = () => {
  <div className={styles["root"]}> heading element </div>
}
