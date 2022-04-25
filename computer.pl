:- ensure_loaded(move).
:- use_module(library(random)).

%choose_move(+GameState, +Level, -Move)
choose_move(GameState, 1, Move):-
    valid_moves(GameState, Moves),
    random_select(Move, Moves, _).

choose_move(gamestate(Board,1,P1,P2),_,Move):-
    valid_moves(gamestate(Board,1,P1,P2), Moves),
    random_select(Move, Moves, _).

choose_move(GameState, 2, Move):-
    current_player(GameState,Player),
    alpha_beta_player(Player,P),
    alpha_beta(1,P,3, GameState, -300, 300, Move, _Value).    

choose_move(GameState, 5, Move):-
    current_player(GameState,Player),
    alpha_beta_player(Player,P),
    alpha_beta(2,P,3, GameState, -300, 300, Move, _Value).

choose_move(GameState, 3, Move):-
    current_player(GameState,Player),
    alpha_beta_player(Player,P),
    alpha_beta(0,P,2, GameState, -300, 300, Move, _Value).     

choose_move(GameState, 6, Move):-
    current_player(GameState,Player),
    alpha_beta_player(Player,P),
    alpha_beta(0,P,3, GameState, -300, 300, Move, _Value).    

choose_move(GameState, 7, Move):-
    current_player(GameState,Player),
    alpha_beta_player(Player,P),
    alpha_beta(3,P,3, GameState, -300, 300, Move, _Value).

choose_move(GameState, 4, Move):-
    current_player(GameState,Player),
    alpha_beta_player(Player,P),
    alpha_beta(3,P,2, GameState, -300, 300, Move, _Value).

%alpha_beta_player(+Player,-AlphaPlayer)
alpha_beta_player(1,1).
alpha_beta_player(2,-1).

%game_over(+GameState, -Winner)
game_over(GameState,1):-
    consecutive_cubes(GameState, 1, 3),!.

game_over(gamestate(_,_,8,_),1):- !.

game_over(gamestate(_,_,_,8),2):- !.

game_over(GameState,2):-
    consecutive_cubes(GameState, 2, 3),!.

%alpha_beta(+ValueFun, +Player, +Depth, +Position, +Alpha, +Beta, -Move, -Value)
alpha_beta(Level,Player,_,Position,_Alpha,_Beta,Move,Value) :- 
    move(Position,Move,NewPosition),
    game_over(NewPosition,_),!,
    value(Level,Position,V),
    Value is V*Player.

alpha_beta(Level,Player,0,Position,_Alpha,_Beta,_NoMove,Value) :- 
   value(Level,Position,V),
   Value is V*Player.

alpha_beta(ValueFun,Player,D,Position,Alpha,Beta,Move,Value) :- 
    D > 0,
    D1 is D-1,
    findall(M,move(Position,M,_NewPosition),Moves), 
    alpha_beta(ValueFun,Player, Moves, Position, D1, Alpha, Beta, nil, Value, Move).

%alpha_beta(+ValueFun, +Player, +Moves, +Position, +Depth, +Alpha, +Beta, +CurrentBestMove, -BestValue, -BestMove)
alpha_beta(_,_, [], _, _, Alpha, _, BestMove, Alpha, BestMove).

alpha_beta(ValueFun,Player, [Move|Moves], Position, D, Alpha, Beta, CurrentBestMove, BestValue, BestMove):-
    move(Position, Move, Position1),
    Opponent is -Player,
    OppAlpha is -Beta,
    OppBeta is -Alpha,
    alpha_beta(ValueFun,Opponent, D, Position1, OppAlpha, OppBeta, _OppMove, OppValue),
    Value is -OppValue,
    (
        Value >= Beta -> BestValue = Value, BestMove = Move;
        Value > Alpha -> alpha_beta(ValueFun,Player, Moves, Position, D, Value, Beta, Move, BestValue, BestMove);
        alpha_beta(ValueFun,Player, Moves, Position, D, Alpha, Beta, CurrentBestMove, BestValue, BestMove)
    ).

%value(+ValueFun, +GameState, -Value)
value(_,Gamestate, 200):-
    consecutive_cubes(Gamestate, 1, 3),!.

value(_,gamestate(_,_,8,_),200).

value(_,Gamestate, -200):-
    consecutive_cubes(Gamestate, 2, 3),!.

value(_,gamestate(_,_,_,8),-200).

value(0,gamestate(Board,T,P1,P2), Value):-
    findall(1,consecutive_cubes(gamestate(Board,T,P1,P2),1,2),L1),
    findall(2,consecutive_cubes(gamestate(Board,T,P1,P2),2,2),L2),
    length(L1,C1),
    length(L2,C2),
    Value is 4*(C1-C2)+(P1-P2).

value(1,gamestate(Board,T,P1,P2),Value):-
    findall(1,consecutive_cubes(gamestate(Board,T,P1,P2),1,2),L1),
    findall(2,consecutive_cubes(gamestate(Board,T,P1,P2),2,2),L2),
    length(L1,C1),
    length(L2,C2),
    Value is (C1-C2).  

value(2,gamestate(_,_,P1,P2),Value):-
    Value is (P1-P2).

value(3,gamestate(Board,T,P1,P2),Value):-
    findall(1,consecutive_cubes(gamestate(Board,T,P1,P2),1,2),L1),
    findall(2,consecutive_cubes(gamestate(Board,T,P1,P2),2,2),L2),
    length(L1,C1),
    length(L2,C2),
    Value is (C1-C2)+4*(P1-P2).

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

consecutive_cubes(gamestate(Board,_,_,_),Player,2):-
    append(_, [L|_], Board),
    append(_, [Player,Player|_], L).  

consecutive_cubes(gamestate(Board,_,_,_),Player,2):-
    append(_, [C1,C2|_], Board),
    append(Before1, [Player|_], C1),
    append(Before2, [Player|_], C2),
    length(Before1, M), 
    length(Before2, M).

consecutive_cubes(gamestate(Board,_,_,_),Player,2):-
    append(_, [C1,C2|_], Board),
    append(Before1, [Player|_], C1),
    append(Before2, [Player|_], C2),
    length(Before1, M1), 
    length(Before2, M2),
    M2 is M1-1.

consecutive_cubes(gamestate(Board,_,_,_),Player,2):-
    append(_, [C1,C2|_], Board),
    append(Before1, [Player|_], C1),
    append(Before2, [Player|_], C2),
    length(Before1, M1), 
    length(Before2, M2),
    M2 is M1+1.