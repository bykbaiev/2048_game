exception NoRoot

switch ReactDOM.querySelector("#root") {
| Some(root) => ReactDOM.render(<div><App /></div>, root)
| None => raise(NoRoot)
}
