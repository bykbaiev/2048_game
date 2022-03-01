let element = <div> {React.string("Hello World")} </div>

let heading = <h1> {React.string("Overview")} </h1>

@react.component
let make = () => {
  <div> heading element </div>
}
