%%raw("import './reset.css'")
%%raw("import './styles.css'")

@react.component
let make = () => {
  <Recoil.RecoilRoot>
    <div>
      <Board />
    </div>
  </Recoil.RecoilRoot>
}
