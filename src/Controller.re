Js.log("Controller is loaded");

let subscription = UI.UI.keyDownStream
    // |> Rx.Operators.takeWhile(
    //     (value, _) => value < 10,
    //     ~inclusive = false,
    // )
    |> Rx.BehaviorSubject.subscribe(
        ~next = x => Js.log(x)
    );
    // |> Rx.Observable.filter(
    //     value => {
    //         switch (value) {
    //             | Some(value) => Js.log(value)
    //             | None => Js.log("None")
    //         }
    //     }
    // );
