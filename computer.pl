:- ensure_loaded(move).
:- ensure_loaded(gameover).
:- use_module(library(random)).

%choose_move(+GameState, +Level, -Move)
choose_move(GameState, 1, Move):-
    valid_moves(GameState, Moves),
    random_select(Move, Moves, _).


choose_move(GameState, 2, Move):-
    current_player(GameState, Player),
    alpha_beta(3, GameState, -200, 200, Move, _Value).    


evaluate_and_choose([ Move | Moves ], Position, D, Alpha, Beta, Move1, BestMove ) :-
    move( Move, Position, Positionl ),
    alpha_beta( D, Positionl, Alpha, Beta, MoveX, Value ),
    Value1 is -Value,
    cutoff( Move, Value1, D, Alpha, Beta, Moves, Position, Move1, BestMove ).

evaluate_and_choose( [], Position, D, Alpha, Beta, Move, ( Move, Alpha )).

alpha_beta( 0, Position, Alpha, Beta, Move, Value ) :- 
    value( Position, Value ).
    
alpha_beta( D, Position, Alpha, Beta, Move, Value ) :- 
    findall( M, move( Position, M ), Moves ),
    Alphal is -Beta,
    Betal is -Alpha,
    D1 is D-l,
    evaluate_and_choose( Moves, Position, D1, Alphal, Betal, nil, ( Move, Value )).

    
cutoff( Move, Value, D, Alpha, Beta, Moves, Position, Movel, ( Move,Value )) :- 
    Value > Beta.
cutoff(Move, Value, D, Alpha, Beta, Moves, Position, Movel, BestMove ) :- 
    Alpha < Value, Value < Beta,
    evaluate_and_choose( Moves, Position, D, Value, Beta, Move, BestMove ).

cutoff( Move, Value, D, Alpha, Beta, Moves, Position, Movel, BestMove ) :- 
    Value < Alpha,
    evaluate_and_choose( Moves, Position, D, Alpha, Beta, Move1, BestMove ).