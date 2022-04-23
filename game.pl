:- ensure_loaded(move).
:- ensure_loaded(interface).
:- ensure_loaded(computer).

%play
play:- 
    menu.

test(Level1,Level2):-
    play_tests(0,0-0,0-0, Level1,Level2,Score),
    format('~nScore: ', []),
    write(Score).

%play_tests
play_tests(Game,I-J,L1-L2,Level1,Level2, Score):-
    write(I-J),
    write(' : '),
    Game < 36,
    initial_state(GameState),
    move(GameState,I-J, NewGameState),
    play_loop_tests(NewGameState,c+Level2-c+Level1, Winner),
    write(Winner),
    format('~n', []),
    update_score(L1-L2,Winner,LL1-LL2),
    Game1 is Game+1,
    next_move(I-J,II-JJ),
    play_tests(Game1,II-JJ,LL1-LL2, Level1,Level2,Score).

play_tests(50,_,L1-L2, _,_,L1-L2).

next_move(I-J,II-J):-
    I < 5, !,
    II is I+1.

next_move(5-J,0-JJ):-
    JJ is J+1,!.

update_score(L1-L2,1,LL1-L2):-
    LL1 is L1+1.

update_score(L1-L2,2,L1-LL2):-
    LL2 is L2+1.

update_score(L1-L2,0,L1-L2).

%play_loop_tests(+GameState, +Players, -Winner)
play_loop_tests(GameState,_, Winner):-
    game_over(GameState, Winner), !.

play_loop_tests(gamestate(_,100,_,_),_, 0).

play_loop_tests(GameState, P1+L1-P2+L2, Winner):-
    choose_move(GameState,L1,I-J),
    move(GameState, I-J, NewGameState),
    play_loop_tests(NewGameState, P2+L2-P1+L1, Winner).


%menu
menu:-
    repeat,
    display_menu,
    format('Option: ', []), read_digit_between(-1,3,Option),
    menu(Option), !.

%menu(+Option)
menu(0):-
    abort.

menu(1):-
    options.

menu(2):-
    format('~nGekitai DESCRIPTION TBD',[]),
    press_enter_to_continue,
    menu.

%options
options:-
    repeat,
    display_play_options,
    format('Option: ', []), read_digit_between(-1,4,Option),
    play_options(Option), !.

%play_options(+Option)
play_options(0):-
    menu.

play_options(1):-
   play_game(h+0-h+0).

play_options(2):-
    format('Choose who starts the game: ',[]),
    format('~n0 - Human',[]),
    format('~n1 - Computer',[]),
    format('~nOption: ', []), read_digit_between(-1,2,Option),
    format('~nChoose the level of the computer',[]),
    format('~nLevel (1-5): ', []),read_digit_between(0,6,Level),
    play_human_computer(Option,Level).

play_options(3):-
    format('Choose the level of Computer 1: ',[]),
    format('~nLevel (1-5): ', []), read_digit_between(0,6,Level1),
    format('Choose the level of Computer 2: ',[]),
    format('~nLevel (1-5): ', []), read_digit_between(0,6,Level2),
    play_game(c+Level1-c+Level2).



%play_human_computer(+Option, +Level)
play_human_computer(0,Level):-
    play_game(h+0-c+Level).

play_human_computer(1,Level):-
    play_game(c+Level-h+0).

%play_game(+Players)
play_game(P1+L1-P2+L2):-
    initial_state(GameState),
    display_game(GameState),
    play_loop(GameState, P1+L1-P2+L2).

%play_loop(+GameState, +Players)
play_loop(GameState,_):-
    game_over(GameState, Winner), !, 
    display_winner(Winner).

play_loop(GameState, P1+L1-P2+L2):-
    display_valid_moves(GameState, P1),
    select_move(GameState,P1+L1, I-J),
    move(GameState, I-J, NewGameState),
    display_game(NewGameState),
    play_loop(NewGameState, P2+L2-P1+L1).
