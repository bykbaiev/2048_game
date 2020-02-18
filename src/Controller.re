Js.log("Controller is loaded");

let subscription = UI.UI.keyDownStream
    |> Rx.Subject.subscribe(
        ~next = x => Js.log(x)
    );
