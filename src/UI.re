module UI = {
    type keyEvent = UP | RIGHT | LEFT | DOWN;
    
    let keyDownStream: Rx.Subject.t(keyEvent) = Rx.Subject.create();

    HtmlDom.onDOMContentLoaded(() => {
        HtmlDom.updateUI(HtmlDom.getUIMessage("Have fun"));

        Rx.fromEvent(~target = HtmlDom.document, ~eventName = "keydown")
            |> Rx.Operators.map(
                (value, _) => HtmlDom.keyboardEventToJsObj(value)
            )
            |> Rx.Operators.filter(
                (value, _) => {
                    switch (value##keyCode) {
                        | 37 | 38 | 39 | 40 => true
                        | _ => false
                    }
                }
            )
            |> Rx.Operators.map(
                (value, _) => {
                    switch (value##keyCode) {
                        | 37 => LEFT
                        | 38 => UP
                        | 39 => RIGHT
                        | _ => DOWN
                    }
                }
            )
            |> Rx.Observable.subscribe(
                ~next = (value: keyEvent) => {
                    keyDownStream |> Rx.Subject.next(value);
                }
            );
    });

    State.GameState.board |> Rx.BehaviorSubject.subscribe(
        ~next = (value: State.GameState.gameBoard) => {
            value |> State.GameState.mapBoardToJs |> Js.log;
        }
    )
};
