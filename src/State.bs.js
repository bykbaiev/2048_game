// Generated by BUCKLESCRIPT, PLEASE EDIT WITH CARE
'use strict';

var List = require("bs-platform/lib/js/list.js");
var Rxjs = require("rxjs");
var $$Array = require("bs-platform/lib/js/array.js");

function mapBoardToJs(board) {
  return $$Array.of_list(List.map($$Array.of_list, board));
}

var initialBoard = /* :: */[
  /* :: */[
    0,
    /* :: */[
      0,
      /* :: */[
        0,
        /* :: */[
          0,
          /* [] */0
        ]
      ]
    ]
  ],
  /* :: */[
    /* :: */[
      0,
      /* :: */[
        0,
        /* :: */[
          0,
          /* :: */[
            0,
            /* [] */0
          ]
        ]
      ]
    ],
    /* :: */[
      /* :: */[
        0,
        /* :: */[
          0,
          /* :: */[
            0,
            /* :: */[
              0,
              /* [] */0
            ]
          ]
        ]
      ],
      /* :: */[
        /* :: */[
          0,
          /* :: */[
            0,
            /* :: */[
              0,
              /* :: */[
                0,
                /* [] */0
              ]
            ]
          ]
        ],
        /* [] */0
      ]
    ]
  ]
];

var board = new Rxjs.BehaviorSubject(initialBoard);

var GameState = {
  mapBoardToJs: mapBoardToJs,
  initialBoard: initialBoard,
  board: board
};

exports.GameState = GameState;
/* board Not a pure module */
