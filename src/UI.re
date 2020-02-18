module UI = {
    type document;
    type htmlElement;
    type keyEvent = {
        keyCode: int
    };
    type handler = unit => Rx.Subscription.t;
    
    let keyDownStream: Rx.Subject.t(keyEvent) = Rx.Subject.create();

    let domDocument : document = [%bs.raw {| document |}];
    [@bs.send] external getElementById : (document, string) => htmlElement = "getElementById";

    let onDOMContentLoaded : (handler) => unit = [%bs.raw
        {|
            function(handler) {
                document.addEventListener('DOMContentLoaded', handler, false);
            }
        |}
    ];

    let getUIMessage : string => list(string) = text => <>{text}</>;

    let updateUI : (list(string)) => unit = [%bs.raw
        {|
            function(jsx) {
                document.querySelector('#app').innerHTML = jsx;
            }
        |}
    ];

    onDOMContentLoaded(() => {
        updateUI(getUIMessage("Have fun"));

        Rx.fromEvent(~target = domDocument, ~eventName = "keydown")
            |> Rx.Operators.filter(
                (value, _) => {
                    switch (value.keyCode) {
                        | 37 | 38 | 39 | 40 => true
                        | _ => false
                    }
                }
            )
            |> Rx.Observable.subscribe(
                ~next = value => {
                    keyDownStream |> Rx.Subject.next({ keyCode: value.keyCode })
                }
            );
    });
}
