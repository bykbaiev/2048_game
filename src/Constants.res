type direction =
  | Up
  | Right
  | Down
  | Left

let winningValue = 2048

/* 600ms to show the difference in score for previous state and current */
let scoreDifferenceAnimationDuration = 600
