# game-solver-template

# Project Grade:         62/100
## Functionality               38/73
* Game mechanics:              17/20
  * Missing test cases.
  * File format is just show/read, which is not safe nor easy to manipulate, but
    works.
  * Mechanics generally work
* Exact game solver:           2/15
  * whoWillWin/bestMove do not work for either the exact or cut-off depth. it is closer to the cut
    off depth solver, but neither seems implemented.
* Cut-off depth solver:        11/13 
  * This is close, although the player always starts as red for some reason, and the depth is fixed
    at 4.
* Evaluation function:         0/2
  * Just counting pieces tells you nothing about the board state, only how many turns it has been.
* Avoiding unnecessary work:   0/3
* Command-line interface:      4/10
  * Main not implemented, got flags limping and running to test it
* Move and verbose flags:      4/5
  * move flag works, but doesn't output in file format
  * verbose flag doesn't seem to do anything
  * interactive flag is two-player, not against computer (+2)
* Error-handling:              0/5
  * no error handling

## Design                      24/27
* Well-designed data types:    8/8
  * game vs gamestate is very confusing. Perhaps renaming it to board would have been better.
* Well-decomposed functions:   8/10
  * generally quite reasonable, although you have many overly short functions that could likely be
    inlined or helper functions.
  * bestMoveStrategy should not take 'ismaximizingplayer', that's already in the gamestate.
* Good module decomposition:   2/2
  * Pretty good, although flags should be main, and some seem to do nothing.
* Good variable names:         2/2
  * Short, but consistent and appropriate to the type.
* Efficient/idiomatic code:    4/5
  * Usually good, although playGame and processArgs are quite awkward. You do shlemiel your valid
    plays.
