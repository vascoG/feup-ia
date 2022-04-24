:- ensure_loaded(board).
:- use_module(library(between)).

/**
* Game state is represented by the structure gamestate(Board, Turn, P1,P2)
*/

%move(+GameState, +Move, -NewGameState)
move(gamestate(Board,Turn,P1,P2), MoveI-MoveJ, gamestate(NewBoard,NewTurn, NewP1,NewP2)):-
    current_player(gamestate(Board,Turn,P1,P2),Player),
    between(0,5, MoveI), %5 ou 6?
    between(0,5, MoveJ),
    board(Board, MoveI-MoveJ, 0),  %Verificar se a célula está vazia.
    update_board(Board, MoveI,MoveJ, Player, NB),
    NewTurn is Turn +1,
    move_adjacents(NB, Player, MoveI-MoveJ, P1, P2, NewBoard, NewP1, NewP2).

%move_adjacents
move_adjacents(Board, 1, I-J, P1, P2, NewBoard, NewP1, NewP2):-
    I1 is I-1,
    J1 is J-1,
    I2 is I+1,
    J2 is J+1,
    repel(Board, I1-J, I-J,NB, P11, P21),
    repel(NB, I1-J1, I-J, NB1, P12, P22),
    repel(NB1, I-J1, I-J, NB2, P13, P23),
    repel(NB2, I2-J, I-J, NB3, P14, P24),
    repel(NB3, I2-J2, I-J, NB4, P15, P25),
    repel(NB4, I-J2, I-J, NB5, P16, P26),
    repel(NB5, I1-J2, I-J, NB6, P17, P27),
    repel(NB6, I2-J1, I-J, NewBoard, P18, P28),
    NewP1 is P1+P11+P12+P13+P14+P15+P16+P17+P18+1,
    NewP2 is P2+P21+P22+P23+P24+P25+P26+P27+P28.

move_adjacents(Board, 2, I-J, P1, P2, NewBoard, NewP1, NewP2):-
    I1 is I-1,
    J1 is J-1,
    I2 is I+1,
    J2 is J+1,
    repel(Board, I1-J, I-J,NB, P11, P21),
    repel(NB, I1-J1, I-J, NB1, P12, P22),
    repel(NB1, I-J1, I-J, NB2, P13, P23),
    repel(NB2, I2-J, I-J, NB3, P14, P24),
    repel(NB3, I2-J2, I-J, NB4, P15, P25),
    repel(NB4, I-J2, I-J, NB5, P16, P26),
    repel(NB5, I1-J2, I-J, NB6, P17, P27),
    repel(NB6, I2-J1, I-J, NewBoard, P18, P28),
    NewP1 is P1+P11+P12+P13+P14+P15+P16+P17+P18,
    NewP2 is P2+P21+P22+P23+P24+P25+P26+P27+P28+1.




repel(Board, 0-J, 1-_, NewBoard, -1,0):-
    board(Board,0-J,1),
    update_board(Board, 0,J, 0, NewBoard),!.
repel(Board, 0-J, 1-_, NewBoard, 0,-1):-
    board(Board,0-J,2),
    update_board(Board, 0,J, 0, NewBoard),!.
repel(Board, 5-J, 4-_, NewBoard, -1,0):-
    board(Board,5-J,1),
    update_board(Board, 5,J, 0, NewBoard),!.
repel(Board, 5-J, 4-_, NewBoard, 0,-1):-
    board(Board,5-J,2),
    update_board(Board, 5,J, 0, NewBoard),!.
repel(Board, I-0, _-1, NewBoard, -1,0):-
    board(Board,I-0,1),
    update_board(Board, I,0, 0, NewBoard),!.
repel(Board, I-0, _-1, NewBoard, 0,-1):-
    board(Board,I-0,2),
    update_board(Board, I,0, 0, NewBoard),!.
repel(Board, I-5, _-4, NewBoard, -1,0):-
    board(Board,I-5,1),
    update_board(Board, I,5, 0, NewBoard),!.
repel(Board, I-5, _-4, NewBoard, 0,-1):-
    board(Board,I-5,2),
    update_board(Board, I,5, 0, NewBoard),!.

repel(Board, I-J, MoveI-MoveJ, NewBoard,0,0):-
    I>=0,
    J>=0,
    I=<5,
    J=<5,
    DiffI is I-MoveI,
    DiffJ is J-MoveJ,
    NewI is DiffI+I,
    NewJ is DiffJ+J,
    board(Board, NewI-NewJ, 0),
    board(Board,I-J,Player),
    update_board(Board,NewI,NewJ,Player,NB),
    update_board(NB, I,J,0,NewBoard),!.


repel(Board,_,_,Board,0,0).


%ter um default case no fim para o caso de nao entrar em funçoes anteriores de repel em que nao muda a board


%valid_moves(+GameState, -ListOfMoves)
valid_moves(GameState,ListOfMoves):-
    findall(Move, move(GameState, Move ,_), ListOfMoves1),
    sort(ListOfMoves1,ListOfMoves).

%select_move(+GameState, +Player, -I-J)
select_move(_,h+_, I-J):-
    repeat,
    format('~nColumn: ',[]), read_digit_between_one_time(-1,6,I),
    format('~nRow: ',[]), read_digit_between_one_time(-1,6,J).

select_move(GameState,c+Level, Move):-
    choose_move(GameState,Level,Move),
    current_player(GameState, Player),
    format('~nComputer (Player ~d) has chosen to play in: ~w', [Player,Move]),
    press_enter_to_continue.


%current_player(+GameState, -Player)
current_player(gamestate(_,Turn,_,_),Player):-
    Turn1 is (Turn rem 2),
    player_turn(Turn1, Player).

%opponent(+Player,-Opponent)
opponent(1,2).
opponent(2,1).

%player_turn(+Turn, -Player)
player_turn(0, 2).
player_turn(1, 1).
