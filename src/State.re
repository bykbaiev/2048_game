module GameState {
    type gameBoard = list(list(int));

    let mapBoardToJs = (board : gameBoard) => board |> List.map(row => Array.of_list(row)) |> Array.of_list;

    let initialBoard = [
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0],
        [0, 0, 0, 0]
    ];

    let board : Rx.BehaviorSubject.t(gameBoard) = Rx.BehaviorSubject.create(initialBoard);
};
