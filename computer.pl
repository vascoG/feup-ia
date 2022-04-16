:- ensure_loaded(move).
:- use_module(library(random)).

%choose_move(+GameState, +Level, -Move)
choose_move(GameState, 1, Move):-
    valid_moves(GameState, Moves),
    random_select(Move, Moves, _).


choose_move(GameState, 2, Move):-
    current_player(GameState,Player),
    alpha_beta_player(Player,P),
    alpha_beta(P,3, GameState, -200, 200, Move, _Value).    


alpha_beta_player(1,1).
alpha_beta_player(2,-1).

game_over(GameState,1):-
    consecutive_cubes(GameState, 1, 3).


game_over(GameState,2):-
    consecutive_cubes(GameState, 2, 3).


alpha_beta(Player,_,Position,_Alpha,_Beta,_NoMove,Value) :- 
   game_over(Position,_),!,
   value(Position,V),
    Value is V*Player.

alpha_beta(Player,0,Position,_Alpha,_Beta,_NoMove,Value) :- 
   value(Position,V),
   Value is V*Player.

alpha_beta(Player,D,Position,Alpha,Beta,Move,Value) :- 
    D > 0,
    D1 is D-1,
    findall(M,move(Position,M,NewPosition),Moves), 
    alpha_beta(Player, Moves, Position, D1, Alpha, Beta, nil, Value, Move).

alpha_beta(_, [], _, _, Alpha, _, BestMove, Alpha, BestMove).

alpha_beta(Player, [Move|Moves], Position, D, Alpha, Beta, CurrentBestMove, BestValue, BestMove):-
    move(Position, Move, Position1),
    Opponent is -Player,
    OppAlpha is -Beta,
    OppBeta is -Alpha,
    alpha_beta(Opponent, D, Position1, OppAlpha, OppBeta, OppMove, OppValue),
    Value is -OppValue,
    (
        Value >= Beta -> BestValue = Value, BestMove = Move;
        Value > Alpha -> alpha_beta(Player, Moves, Position, D, Value, Beta, Move, BestValue, BestMove);
        alpha_beta(Player, Moves, Position, D, Alpha, Beta, CurrentBestMove, BestValue, BestMove)
    ).




/*
alpha_beta(Player,0,Position,_Alpha,_Beta,_NoMove,Value) :- 
   value(Position,Player,Value).

alpha_beta(Player,D,Position,Alpha,Beta,Move,Value) :- 
   D > 0, 
   findall(M,move(Position,M,NewPosition),Moves), 
   Alpha1 is -Beta, 
   Beta1 is -Alpha,
   D1 is D-1, 
   evaluate_and_choose(Player,Moves,Position,D1,Alpha1,Beta1,nil,(Move,Value)).

evaluate_and_choose(Player,[Move|Moves],Position,D,Alpha,Beta,Record,BestMove) :-
   move(Position,Move,Position1), 
   opponent(Player,OtherPlayer),
   alpha_beta(OtherPlayer,D,Position1,Alpha,Beta,_OtherMove,Value),
   Value1 is -Value,
   cutoff(Player,Move,Value1,D,Alpha,Beta,Moves,Position,Record,BestMove).
evaluate_and_choose(_Player,[],_Position,_D,Alpha,_Beta,Move,(Move,Alpha)).

cutoff(_Player,Move,Value,_D,_Alpha,Beta,_Moves,_Position,_Record,(Move,Value)) :- 
   Value >= Beta.
cutoff(Player,Move,Value,D,Alpha,Beta,Moves,Position,_Record,BestMove) :- 
   Alpha < Value, Value < Beta,  
   evaluate_and_choose(Player,Moves,Position,D,Value,Beta,Move,BestMove).
cutoff(Player,_Move,Value,D,Alpha,Beta,Moves,Position,Record,BestMove) :- 
   Value =< Alpha, 
   evaluate_and_choose(Player,Moves,Position,D,Alpha,Beta,Record,BestMove).

*/

/*
evaluate_and_choose(Player,[ Move | Moves ], Position, D, Alpha, Beta, Move1, BestMove ) :-
    move( Position, Move, Positionl ),
    opponent(Player, Player1),
    alpha_beta( Player1,D, Positionl, Alpha, Beta, _MoveX, Value ),
    Value1 is -Value,
    cutoff(Player, Move, Value1, D, Alpha, Beta, Moves, Position, Move1, BestMove ).

evaluate_and_choose(_Player, [], _Position, _D, Alpha, _Beta, Move, ( Move, Alpha )).

alpha_beta(Player, 0, Position, Alpha, _Beta, _Move, Value ) :- 
    value( Position, Player,Value ).
    
alpha_beta(Player, D, Position, Alpha, Beta, Move, Value ) :- 
    D > 0,
    findall( M, move( Position, M,_ ), Moves ),
    Alphal is -Beta,
    Betal is -Alpha,
    D1 is D-1,
    evaluate_and_choose(Player, Moves, Position, D1, Alphal, Betal, nil, ( Move, Value )).

    
cutoff( _Player,Move, Value, D, Alpha, Beta, Moves, Position, Movel, ( Move,Value )) :- 
    Value >= Beta, !.
cutoff(Player,Move, Value, D, Alpha, Beta, Moves, Position, Movel, BestMove ) :- 
    Alpha < Value, Value < Beta, !,
    evaluate_and_choose(Player, Moves, Position, D, Value, Beta, Move, BestMove ).

cutoff( Player,Move, Value, D, Alpha, Beta, Moves, Position, Movel, BestMove ) :- 
    Value =< Alpha, !,
    evaluate_and_choose(Player, Moves, Position, D, Alpha, Beta, Move1, BestMove ).

*/
value(Gamestate, 100):-
    consecutive_cubes(Gamestate, 1, 3),!.

value(Gamestate, -100):-
    consecutive_cubes(Gamestate, 2, 3),!.

value(_,0).


%consecutive_cubes(+GameState, +Player, +N)
consecutive_cubes(gamestate(Board,_,_,_),Player,3):-
    append(_, [L|_], Board),
    append(_, [Player,Player,Player|_], L). 

consecutive_cubes(gamestate(Board,_,_,_),Player,3):-
    append(_, [C1,C2,C3|_], Board),
    append(Before1, [Player|_], C1),
    append(Before2, [Player|_], C2),
    append(Before3, [Player|_], C3),
    length(Before1, M), 
    length(Before2, M),
    length(Before3, M).

consecutive_cubes(gamestate(Board,_,_,_),Player,3):-
    append(_, [C1,C2,C3|_], Board),
    append(Before1, [Player|_], C1),
    append(Before2, [Player|_], C2),
    append(Before3, [Player|_], C3),
    length(Before1, M1), 
    length(Before2, M2),
    length(Before3, M3),
    M2 is M1-1, M3 is M2-1.


consecutive_cubes(gamestate(Board,_,_,_),Player,3):-
    append(_, [C1,C2,C3|_], Board),
    append(Before1, [Player|_], C1),
    append(Before2, [Player|_], C2),
    append(Before3, [Player|_], C3),
    length(Before1, M1), 
    length(Before2, M2),
    length(Before3, M3),
    M2 is M1+1, M3 is M2+1.