:- use_module(library(lists)).

/**
* Board is represented by a list of lists that can be queried using predicate board(+Board,+I-J,-Player), meaning in position (I,J) there is
* a piece placed by the Player. If there is no cubes, Player returns 0.
*/

%initial_state(-GameState)
initial_state(gamestate(Board,1,0,0)):- initial_board( Board).

%initial_board( -Board)
initial_board(Board):- 
    create_board(6,6,Board).

%create_board(+SizeLine,+SizeColumn, -Board)
create_board(_,0,[]).

create_board(Size,Column,[Line|Board]):-
    Column > 0,
    Column1 is Column-1,
    create_line(Size,Line),
    create_board(Size,Column1,Board).

%create_line(+Size, -Line)
create_line(1,[0]).

create_line(Size, Line):- 
    Size > 1,
    Size1 is Size-1,
    create_line(Size1,Line1),
    append(Line1,[0],Line).

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




