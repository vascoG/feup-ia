# IA

## Game state's representation

### Board

6x6 Matrix, 0 represents an empty space, 1 represents Player1's pieces and 2 represents Player2's pieces.

### Game State

gamestate(Board, Turn, P1Pieces, P2Pieces)

Turn is 1,2,3,4,5...
P1/P2Pieces is the total number of pieces that each Player has on board

### Winner conditions

The first Player to either line up 3 pieces in a row or have all eight pieces on the board wins the game.
