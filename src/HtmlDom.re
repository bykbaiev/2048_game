[@bs.val] external document : Dom.document = "";

[@bs.send] external getElementById : (Dom.document, string) => Dom.element = "getElementById";

external keyboardEventToJsObj : Dom.keyboardEvent => Js.t({..}) = "%identity";

type handler = unit => Rx.Subscription.t;

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
        function(update) {
            document.querySelector('#app').innerHTML = update;
        }
    |}
];
