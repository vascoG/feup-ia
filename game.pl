:- ensure_loaded(move).
:- ensure_loaded(interface).

%play
play:- 
    menu.

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
    format('~nLevel (1 or 2): ', []),read_digit_between(0,3,Level),
    play_human_computer(Option,Level).

play_options(3):-
    format('Choose the level of Computer 1: ',[]),
    format('~nLevel (1 or 2): ', []), read_digit_between(0,3,Level1),
    format('Choose the level of Computer 2: ',[]),
    format('~nLevel (1 or 2): ', []), read_digit_between(0,3,Level2),
    play_game(c+Level1-c+Level2).



%play_human_computer(+Option, +Level)
play_human_computer(0,Level):-
    play_game(h+0-c+Level).

play_human_computer(1,Level):-
    play_game(c+Level-h+0).

%play_game(+Players)
play_game(P1+L1-P2+L2):-
    initial_state(Size, GameState),
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
