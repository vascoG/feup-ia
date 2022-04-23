:- use_module(library(lists)).


%initial_state(-GameState)
initial_state(gamestate([[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0],[0,0,0,0,0,0]],1,0,0)).

%board(+Board,+I-J,?Player)
board(Board,I-J,Player):-
    nth0(J, Board, Line),
    nth0(I, Line, Player).

%update_board(+Board, +I, +J, +Player, -NewBoard)
update_board([[_|Line]|Board],0,0,Player,[[Player|Line]|Board]):- !.
update_board([[E|Line]|Board],I,0,Player,[[E|NewLine]|Board]):- 
    I1 is I-1,
    update_board([Line|Board],I1,0,Player,[NewLine|Board]).
update_board([Line|Board],I,J,Player,[Line|NewBoard]):- 
    J1 is J-1,
    update_board(Board,I,J1,Player, NewBoard).




