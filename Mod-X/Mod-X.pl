:- use_module(library(pce)).
:- use_module(library(lists)).
:- use_module(library(random)).

box(21,21,11).
box(71,21,12).
box(121,21,13).
box(171,21,14).
box(221,21,15).
box(271,21,16).
box(321,21,17).
box(371,21,18).

box(21,71,21).
box(71,71,22).
box(121,71,23).
box(171,71,24).
box(221,71,25).
box(271,71,26).
box(321,71,27).
box(371,71,28).

box(21,121,31).
box(71,121,32).
box(121,121,33).
box(171,121,34).
box(221,121,35).
box(271,121,36).
box(321,121,37).
box(371,121,38).

box(21,171,41).
box(71,171,42).
box(121,171,43).
box(171,171,44).
box(221,171,45).
box(271,171,46).
box(321,171,47).
box(371,171,48).

box(21,221,51).
box(71,221,52).
box(121,221,53).
box(171,221,54).
box(221,221,55).
box(271,221,56).
box(321,221,57).
box(371,221,58).

box(21,271,61).
box(71,271,62).
box(121,271,63).
box(171,271,64).
box(221,271,65).
box(271,271,66).
box(321,271,67).
box(371,271,68).

box(21,321,71).
box(71,321,72).
box(121,321,73).
box(171,321,74).
box(221,321,75).
box(271,321,76).
box(321,321,77).
box(371,321,78).

box(21,371,81).
box(71,371,82).
box(121,371,83).
box(171,371,84).
box(221,371,85).
box(271,371,86).
box(321,371,87).
box(371,371,88).





box(11,@bo11).
box(12,@bo12).
box(13,@bo13).
box(14,@bo14).
box(15,@bo15).
box(16,@bo16).
box(17,@bo17).
box(18,@bo18).

box(21,@bo21).
box(22,@bo22).
box(23,@bo23).
box(24,@bo24).
box(25,@bo25).
box(26,@bo26).
box(27,@bo27).
box(28,@bo28).

box(31,@bo31).
box(32,@bo32).
box(33,@bo33).
box(34,@bo34).
box(35,@bo35).
box(36,@bo36).
box(37,@bo37).
box(38,@bo38).

box(41,@bo41).
box(42,@bo42).
box(43,@bo43).
box(44,@bo44).
box(45,@bo45).
box(46,@bo46).
box(47,@bo47).
box(48,@bo48).

box(51,@bo51).
box(52,@bo52).
box(53,@bo53).
box(54,@bo54).
box(55,@bo55).
box(56,@bo56).
box(57,@bo57).
box(58,@bo58).

box(61,@bo61).
box(62,@bo62).
box(63,@bo63).
box(64,@bo64).
box(65,@bo65).
box(66,@bo66).
box(67,@bo67).
box(68,@bo68).

box(71,@bo71).
box(72,@bo72).
box(73,@bo73).
box(74,@bo74).
box(75,@bo75).
box(76,@bo76).
box(77,@bo77).
box(78,@bo78).

box(81,@bo81).
box(82,@bo82).
box(83,@bo83).
box(84,@bo84).
box(85,@bo85).
box(86,@bo86).
box(87,@bo87).
box(88,@bo88).


draw():-
new(@gamer, picture('== MOD X :: GAME ==',size(442,484))),send(@gamer, open_centered),
send(@gamer, display,new(@bo11, box(50,50)),point(21,21)),
send(@gamer, display,new(@bo12, box(50,50)),point(71,21)),
send(@gamer, display,new(@bo13, box(50,50)),point(121,21)),
send(@gamer, display,new(@bo14, box(50,50)),point(171,21)),
send(@gamer, display,new(@bo15, box(50,50)),point(221,21)),
send(@gamer, display,new(@bo16, box(50,50)),point(271,21)),
send(@gamer, display,new(@bo17, box(50,50)),point(321,21)),
send(@gamer, display,new(@bo18, box(50,50)),point(371,21)),

send(@gamer, display,new(@bo21, box(50,50)),point(21,71)),
send(@gamer, display,new(@bo22, box(50,50)),point(71,71)),
send(@gamer, display,new(@bo23, box(50,50)),point(121,71)),
send(@gamer, display,new(@bo24, box(50,50)),point(171,71)),
send(@gamer, display,new(@bo25, box(50,50)),point(221,71)),
send(@gamer, display,new(@bo26, box(50,50)),point(271,71)),
send(@gamer, display,new(@bo27, box(50,50)),point(321,71)),
send(@gamer, display,new(@bo28, box(50,50)),point(371,71)),

send(@gamer, display,new(@bo31, box(50,50)),point(21,121)),
send(@gamer, display,new(@bo32, box(50,50)),point(71,121)),
send(@gamer, display,new(@bo33, box(50,50)),point(121,121)),
send(@gamer, display,new(@bo34, box(50,50)),point(171,121)),
send(@gamer, display,new(@bo35, box(50,50)),point(221,121)),
send(@gamer, display,new(@bo36, box(50,50)),point(271,121)),
send(@gamer, display,new(@bo37, box(50,50)),point(321,121)),
send(@gamer, display,new(@bo38, box(50,50)),point(371,121)),

send(@gamer, display,new(@bo41, box(50,50)),point(21,171)),
send(@gamer, display,new(@bo42, box(50,50)),point(71,171)),
send(@gamer, display,new(@bo43, box(50,50)),point(121,171)),
send(@gamer, display,new(@bo44, box(50,50)),point(171,171)),
send(@gamer, display,new(@bo45, box(50,50)),point(221,171)),
send(@gamer, display,new(@bo46, box(50,50)),point(271,171)),
send(@gamer, display,new(@bo47, box(50,50)),point(321,171)),
send(@gamer, display,new(@bo48, box(50,50)),point(371,171)),

send(@gamer, display,new(@bo51, box(50,50)),point(21,221)),
send(@gamer, display,new(@bo52, box(50,50)),point(71,221)),
send(@gamer, display,new(@bo53, box(50,50)),point(121,221)),
send(@gamer, display,new(@bo54, box(50,50)),point(171,221)),
send(@gamer, display,new(@bo55, box(50,50)),point(221,221)),
send(@gamer, display,new(@bo56, box(50,50)),point(271,221)),
send(@gamer, display,new(@bo57, box(50,50)),point(321,221)),
send(@gamer, display,new(@bo58, box(50,50)),point(371,221)),

send(@gamer, display,new(@bo61, box(50,50)),point(21,271)),
send(@gamer, display,new(@bo62, box(50,50)),point(71,271)),
send(@gamer, display,new(@bo63, box(50,50)),point(121,271)),
send(@gamer, display,new(@bo64, box(50,50)),point(171,271)),
send(@gamer, display,new(@bo65, box(50,50)),point(221,271)),
send(@gamer, display,new(@bo66, box(50,50)),point(271,271)),
send(@gamer, display,new(@bo67, box(50,50)),point(321,271)),
send(@gamer, display,new(@bo68, box(50,50)),point(371,271)),

send(@gamer, display,new(@bo71, box(50,50)),point(21,321)),
send(@gamer, display,new(@bo72, box(50,50)),point(71,321)),
send(@gamer, display,new(@bo73, box(50,50)),point(121,321)),
send(@gamer, display,new(@bo74, box(50,50)),point(171,321)),
send(@gamer, display,new(@bo75, box(50,50)),point(221,321)),
send(@gamer, display,new(@bo76, box(50,50)),point(271,321)),
send(@gamer, display,new(@bo77, box(50,50)),point(321,321)),
send(@gamer, display,new(@bo78, box(50,50)),point(371,321)),

send(@gamer, display,new(@bo81, box(50,50)),point(21,371)),
send(@gamer, display,new(@bo82, box(50,50)),point(71,371)),
send(@gamer, display,new(@bo83, box(50,50)),point(121,371)),
send(@gamer, display,new(@bo84, box(50,50)),point(171,371)),
send(@gamer, display,new(@bo85, box(50,50)),point(221,371)),
send(@gamer, display,new(@bo86, box(50,50)),point(271,371)),
send(@gamer, display,new(@bo87, box(50,50)),point(321,371)),
send(@gamer, display,new(@bo88, box(50,50)),point(371,371)),

send(@gamer, display,new(@scrore1, text('0')), point(131, 440)),
send(@gamer, display,new(@scrore2, text('0')), point(331, 440)),
send(@gamer, display,new(@txtscrore1, text('Player 1 Score: ')), point(21, 440)),
send(@gamer, display,new(@txtscrore2, text('Player 2 Score: ')), point(221, 440)),
send(@gamer, display,new(@info, text('BOARD')), point(21, 5)),
send(@scrore1, font, font(times, bold, 15)),
send(@scrore2, font, font(times, bold, 15)),
send(@txtscrore1, font, font(times, bold, 14)),
send(@txtscrore2, font, font(times, bold, 14)),
send(@info, font, font(times, normal, 12)).

redraw(Box,0):-
send(Box, fill_pattern, colour(white)).
redraw(Box,1):-
send(Box, fill_pattern, colour(cyan)).
redraw(Box,2):-
send(Box, fill_pattern, colour(magenta)).


markx(Box,1):-
get(Box, position, Pos),
get(Pos,x,X),
get(Pos,y,Y),
Xend is X+50,
Yend is Y+50,
send(@gamer, display, new(L1,line(X,Y,Xend,Yend,none))),
send(@gamer, display, new(L2,line(Xend,Y,X,Yend,none))),
send(L1,colour(blue)),
send(L2,colour(blue)).

markx(Box,2):-
get(Box, position, Pos),
get(Pos,x,X),
get(Pos,y,Y),
Xend is X+50,
Yend is Y+50,
send(@gamer, display, new(L1,line(X,Y,Xend,Yend,none))),
send(@gamer, display, new(L2,line(Xend,Y,X,Yend,none))),
send(L1,colour(red)),
send(L2,colour(red)).

markx(Box,3):-
get(Box, position, Pos),
get(Pos,x,X),
get(Pos,y,Y),
Xend is X+50,
Yend is Y+50,
send(@gamer, display, new(L1,line(X,Y,Xend,Yend,none))),
send(@gamer, display, new(L2,line(Xend,Y,X,Yend,none))),
send(L1,colour(yellow)),
send(L2,colour(yellow)).

redrawx(Box,1):-
get(Box, position, Pos),
get(Pos,x,X),
get(Pos,y,Y),
Xend is X+50,
Yend is Y+50,
send(@gamer, display, new(L1,line(X,Y,Xend,Yend,none))),
send(@gamer, display, new(L2,line(Xend,Y,X,Yend,none))),
send(L1,colour(cyan)),
send(L2,colour(cyan)).

redrawx(Box,2):-
get(Box, position, Pos),
get(Pos,x,X),
get(Pos,y,Y),
Xend is X+50,
Yend is Y+50,
send(@gamer, display, new(L1,line(X,Y,Xend,Yend,none))),
send(@gamer, display, new(L2,line(Xend,Y,X,Yend,none))),
send(L1,colour(magenta)),
send(L2,colour(magenta)).

clearx(Box):-
get(Box, position, Pos),
get(Pos,x,X),
get(Pos,y,Y),
Xend is X+50,
Yend is Y+50,
send(@gamer, display, new(L1,line(X,Y,Xend,Yend,none))),
send(@gamer, display, new(L2,line(Xend,Y,X,Yend,none))),
send(L1,colour(white)),
send(L2,colour(white)).

exit_game:-
free(@gamer),
free(@bo11),
free(@bo12),
free(@bo13),
free(@bo14),
free(@bo15),
free(@bo16),
free(@bo17),
free(@bo18),
free(@bo21),
free(@bo22),
free(@bo23),
free(@bo24),
free(@bo25),
free(@bo26),
free(@bo27),
free(@bo28),
free(@bo31),
free(@bo32),
free(@bo33),
free(@bo34),
free(@bo35),
free(@bo36),
free(@bo37),
free(@bo38),
free(@bo41),
free(@bo42),
free(@bo43),
free(@bo44),
free(@bo45),
free(@bo46),
free(@bo47),
free(@bo48),
free(@bo51),
free(@bo52),
free(@bo53),
free(@bo54),
free(@bo55),
free(@bo56),
free(@bo57),
free(@bo58),
free(@bo61),
free(@bo62),
free(@bo63),
free(@bo64),
free(@bo65),
free(@bo66),
free(@bo67),
free(@bo68),
free(@bo71),
free(@bo72),
free(@bo73),
free(@bo74),
free(@bo75),
free(@bo76),
free(@bo77),
free(@bo78),
free(@bo81),
free(@bo82),
free(@bo83),
free(@bo84),
free(@bo85),
free(@bo86),
free(@bo87),
free(@bo88),
free(@info),
free(@scrore1),
free(@scrore2),
free(@txtscrore1),
free(@txtscrore2).

write_to_screen(0):-
	write('  ').
write_to_screen(1):-
	write(' j').
write_to_screen(2):-
	ansi_format([bold,fg(green)], '1j', [o]).
write_to_screen(3):-
	ansi_format([bold,fg(blue)], '2j', [o]).
write_to_screen(4):-
	write(' x').
write_to_screen(5):-
	ansi_format([bold,fg(green)], '1x', [o]).
write_to_screen(6):-
	ansi_format([bold,fg(blue)], '2x', [o]).
write_to_screen(7):-
	write(' o').
write_to_screen(8):-
	ansi_format([bold,fg(green)], '1o', [o]).
write_to_screen(9):-
	ansi_format([bold,fg(blue)], '2o', [o]).
write_to_screen(10):-
	ansi_format([bold,fg(green)], '1 ', [o]).
write_to_screen(11):-
	ansi_format([bold,fg(blue)], '2 ', [o]).

valid_position(Value):-
	Value=:=0;
	Value=:=10;
	Value=:=11.

player_points_convert(1,10).
player_points_convert(2,11).

play :-
	initial_menu(18,14,1).
	

initial_menu(MaxPoints,MaxPieces,Dificulty):-
	repeat,
	write('1 - Jogo Humano vs Humano'),nl,
	write('2 - Jogo Humano vs Computador'),nl,
	write('3 - Jogo Computador vs Computador'),nl,
	write('4 - Definicoes: escolher numero maximo de pontos'),nl,
	write('5 - Definicoes: escolher numero maximo de pecas para cada jogador'),nl,
	write('6 - Definicoes: escolher dificuldade do Computador'),nl,
	write('0 - Sair do jogo'),nl,
	read(Option),
	valid_input_menu(Option),
	execute(Option,MaxPoints,MaxPieces,Dificulty).
	
execute(Option,MaxPoints,MaxPieces,Dificulty):-
	Option =:= 0, abort;
	Option =:= 1, choose_names(MaxPoints,MaxPieces);
	Option =:= 2, choose_name(MaxPoints,MaxPieces,Dificulty);
	Option =:= 3, choose_different_dificulty(MaxPoints,MaxPieces,Dificulty);
	Option =:= 4, choose_max_points(MaxPoints,MaxPieces,Dificulty);
	Option =:= 5, choose_max_pieces(MaxPoints,MaxPieces,Dificulty);
	Option =:= 6, choose_difficulty(MaxPoints,MaxPieces,Dificulty).
	
	
choose_names(MaxPoints,MaxPieces):-
	write('Introduza o nome do Jogador 1:'),nl,
	read(Name1),
	write('Introduza o nome do Jogador 2:'),nl,
	read(Name2),
	play(1,MaxPoints,_,_,MaxPieces,Name1,Name2).
	
choose_name(MaxPoints,MaxPieces,Dificulty):-
	write('Introduza o nome do Jogador 1:'),nl,
	read(Name1),
	play(2,MaxPoints,Dificulty,_,MaxPieces,Name1,_).	
	
choose_different_dificulty(MaxPoints,MaxPieces,Dificulty):-
	repeat,
	write('Deseja a mesma dificuldade, escolhida previamente, para ambos os jogadores (insira 1), ou dificuldades diferentes (insira 2)? '),nl,
	read(Choice),
	number(Choice),
	Choice > 0,
	Choice < 3,
	(
	Choice =:= 1, play(3,MaxPoints,Dificulty,Dificulty,MaxPieces,_,_);
	Choice =:= 2, play(3,MaxPoints,1,2,MaxPieces,_,_)
	).
	
	
choose_max_points(MaxPoints,MaxPieces,Dificulty):-
	repeat,
	write('Atualmente, o jogo termina aos '), write(MaxPoints), write(' pontos.'),nl,
	write('Introduza o numero maximo de pontos que pretende:'),nl,
	read(Points),
	number(Points),
	Points > 0,
	initial_menu(Points,MaxPieces,Dificulty).
	
	
choose_max_pieces(MaxPoints,MaxPieces,Dificulty):-
	repeat,
	write('Atualmente, o jogo termina as '), write(MaxPieces), write(' pecas.'),nl,
	write('Introduza o numero maximo de pecas para cada jogador:'),nl,
	read(Pieces),
	number(Pieces),
	Pieces > 0,
	initial_menu(MaxPoints,Pieces,Dificulty).
	
choose_difficulty(MaxPoints,MaxPieces,Dificulty):-
	repeat,
	write('Atualmente, a dificuldade de jogo e '), write(Dificulty), write('.'),nl,
	write('Introduza a dificuldade de jogo do Computador que pretende (1 ou 2):'),nl,
	read(NewDificulty),
	number(NewDificulty),
	NewDificulty > 0,
	NewDificulty < 3,
	initial_menu(MaxPoints,MaxPieces,NewDificulty).

play(GameType,MaxPoints,Dificulty1,Dificulty2,MaxPieces,Name1,Name2):-
	get_ini_board(Board),
	draw,
	get_jokers(Board,1,NewBoard),
    nl,
    write('==================================='), nl,
	write('============= Mod-X ==============='), nl,
	write('==================================='), nl, nl,
	write('Jogo acaba aos '), write(MaxPoints), write(' pontos ou as '), write(MaxPieces),
	write(' pecas.'),nl,nl,
	printt(NewBoard),
	(
	GameType =:= 1, human_vs_human_play(NewBoard,1,MaxPoints,MaxPieces,Name1,Name2);
	GameType =:= 2, human_vs_computer_play(NewBoard,1,MaxPoints,MaxPieces,Dificulty1,Name1);
	GameType =:= 3, computer_vs_computer_play(NewBoard,1,MaxPoints,MaxPieces,Dificulty1,Dificulty2)
	).
	

valid_input_menu(Pos):-
	Pos >= 0,
	Pos =< 6.
	
get_name(Player,Name1,Name2,CurrentName):-
	Player =:= 1,
	string_concat(Name1,'',CurrentName);
	Player =:= 2,
	string_concat(Name2,'',CurrentName).
	
human_vs_human_play(Board,Player,MaxPoints,MaxPieces,Name1,Name2) :-
	nl,
	repeat,
	get_name(Player,Name1,Name2,CurrentName),
	write(CurrentName),write(', e a tua vez de jogar!'),nl,
	write('Qual a posicao da nova jogada (linha e coluna)? '), nl,
	read(Pos),
	valid_input(Pos),
	argvl(Pos,Line,Column),
	get_value(Board,Line,Column,Value),
	valid_position(Value),
	get_player_arg(Player,Value,A),
	get_new_board(Board,Pos,A,NBoard,Player),
	printt(NBoard),
	verify_patterns(NBoard,Player,Points),
	print_points(Points),
	update_points(NBoard,Points,Player,UBoard),
	count_jokers(UBoard,1,Jokers),
	ask_jokers(UBoard,Player,CurrentName,Jokers,FinalBoard),
	printt(FinalBoard),
	count_points(FinalBoard,1,Points1,Points2),
	send(@scrore1,string(Points1)),
	send(@scrore2,string(Points2)),
	write_points(Points1,Points2,Name1,Name2),
	verify_endgame(FinalBoard,Player,CurrentName,Points1,Points2,MaxPoints,MaxPieces),
	get_player(Player, NPlayer),
	human_vs_human_play(FinalBoard,NPlayer,MaxPoints,MaxPieces,Name1,Name2).
	
convert_joker_position(1,0).
convert_joker_position(2,10).
convert_joker_position(3,11).

update_points(L,[],_,L).
update_points(Board,[H|T],Player,NewBoard):-
	nth1(1,H,X),
	nth1(2,H,Y),
	get_value(Board,X,Y,Value),
	(
		Value >= 1, Value =< 3,
		Pos is X*10+Y,
		convert_joker_position(Value,Substitute),
		get_new_board(Board,Pos,Substitute,NextBoard,Player),
		update_points(NextBoard,T,Player,NewBoard)
	);
	nth1(1,H,X),
	nth1(2,H,Y),
	Pos is X*10+Y,
	set_points(Board,Pos,Player,NextBoard),
	update_points(NextBoard,T,Player,NewBoard).
	
set_points(L,[],_,L).
set_points(Board,Pos,Player,NewBoard):-
	player_points_convert(Player,A),
	get_new_board(Board,Pos,A,NewBoard,Player).
	

print_points([]):- nl.
print_points([H|T]):-
	print_points_line(H),
	print_points(T).

print_points_line([]).
print_points_line([H|T]):-
write('Linha:'),write(H),print_points_line_t(T).

print_points_line_t([]).
print_points_line_t([H|_]):-
write(' Coluna:'),write(H),nl.
	
	
get_player(P, NP):-
	P =:= 1,
	NP is 2;
	P =:= 2,
	NP is 1.
	
get_player_arg(P,Value,A):-
	P =:= 1,
	(
		(Value =:= 0,
		A is 4);
		(Value =:= 10,
		A is 5);
		(Value =:= 11,
		A is 6)
	);
	P =:= 2,
	(
		(Value =:= 0,
		A is 7);
		(Value =:= 10,
		A is 8);
		(Value =:= 11,
		A is 9)
	).

get_jokers(X,6,X):- !.
get_jokers(Board,NJokers,NewBoard):-
	get_joker(Board,NextNewBoard),
	NextNJokers is NJokers+1,
	get_jokers(NextNewBoard,NextNJokers,NewBoard).
	
get_joker(Board,NewBoard):-
	repeat,
	random(1,8,LineIndex),
	random(1,8,ColumnIndex),
	\+ get_value(Board,LineIndex,ColumnIndex,1),
	get_line(LineIndex,Board,Line),
	replace(Line,ColumnIndex,1,NewLine),
	replace(Board,LineIndex,NewLine,NewBoard),
	N is LineIndex*10+ColumnIndex,
	box(N,BO),
	markx(BO,3).
	
printt(Board):-
	write('  | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 |'),nl,
	write('  ---------------------------------'),nl,
	printt_number(Board,1,1).
		
	
printt_number([],9,9).
printt_number([H|T],L,C):-
	L < 9,
	C < 9,
	write(L),write(' | '),
	print_line(H), nl,
	write('  |-------------------------------|'),nl,
	NL is L+1,
	NC is C+1,
	printt_number(T,NL,NC).
	
print_line([]).
print_line([H|T]):-
	write_to_screen(H),
	%write(X),
	write('| '),
	print_line(T).

	
valid_input(X):-
	number(X),
	X =:= 99,exit_game,
	abort;
	X > 10,
	X < 89,
	X\=19,X\=20,X\=29,X\=30,X\=39,X\=40,X\=49,X\=50,X\=59,X\=60,X\=69,X\=70,X\=79,X\=80.
	
get_ini_board(B):- 
	B = [
			[0,0,0,0,0,0,0,0],
			[0,0,0,0,0,0,0,0],
			[0,0,0,0,0,0,0,0],
			[0,0,0,0,0,0,0,0],
			[0,0,0,0,0,0,0,0],
			[0,0,0,0,0,0,0,0],
			[0,0,0,0,0,0,0,0],
			[0,0,0,0,0,0,0,0]
		].
		
argvl(A,X,Y):-
		number(A),
		X is div(A,10),
		Y is mod(A,10).
		
get_line(1,[X|_],X).
get_line(N,[_|T],L):-
		N > 1,
		N1 is N - 1,
		get_line(N1,T,L).
		
replace([_|T], 1, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 1,
    I1 is I - 1,
    replace(T, I1, X, R).
	
	
get_new_board(B,N,A,NB,P):-
	argvl(N,LI,CO),
	get_line(LI,B,L),	
	replace(L,CO,A,NL),
	replace(B,LI,NL,NB),
	update_graphical(N,P,A).

update_graphical(N,_,A):-
	A =:= 0,
	box(N,BO),
	redraw(BO,0),
	clearx(BO).		
	
update_graphical(N,_,A):-
	A >= 1, A =< 3,
	box(N,BO),
	markx(BO,3).
	
update_graphical(N,P,A):-
	A >= 4, A =< 9,
	box(N,BO),
	markx(BO,P).
	
update_graphical(N,P,A):-
	A >= 10, A =< 11,
	box(N,BO),
	redraw(BO,P),
	redrawx(BO,P).
	
update_graphical(_,_,_).

get_value(Board,X,Y,Value):-
	nth1(X,Board,Line),
	nth1(Y,Line,Value).
	
get_value_line(Line,X,Value):-
	nth1(X,Line,Value).	

remove_duplicates([],[]).
remove_duplicates([H|T],[H|R]) :- 
	delete(T,H,S),
	remove_duplicates(S,R).

add_to_list(X, Y, Z) :-
    append(X, Y, Z1),
    remove_duplicates(Z1, Z).
	
verify_patterns(_,_,_,5,_).	
verify_patterns(Board,Player,Points):-
	verify_vertical_line(Board,3,1,Player,PointsVertical),
	add_to_list(PointsVertical,[],NewPoints1),
	verify_horizontal_line(Board,1,3,Player,PointsHorizontal),
	add_to_list(NewPoints1,PointsHorizontal,NewPoints2),
	verify_diagonal_line_crescent(Board,3,3,Player,PointsDiagonalCres),
	add_to_list(NewPoints2,PointsDiagonalCres,NewPoints3),
	verify_diagonal_line_decrescent(Board,3,3,Player,PointsDiagonalDecres),
	add_to_list(NewPoints3,PointsDiagonalDecres,NewPoints4),
	verify_x(Board,2,2,Player,PointsX),
	add_to_list(NewPoints4,PointsX,NewPoints5),
	verify_plus(Board,2,2,Player,PointsPlus),
	add_to_list(NewPoints5,PointsPlus,Points).

verify_plus(_,8,_,1,_).
verify_plus(Board,LineIndex,8,1,Points):-
	NewLineIndex is LineIndex+1,
	verify_plus(Board,NewLineIndex,2,1,Points).
verify_plus(Board,LineIndex,ColumnIndex,1,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex,ColumnIndex2,P2),
	ColumnIndex3 is ColumnIndex+1,
	get_value(Board,LineIndex,ColumnIndex3,P3),
	LineIndex4 is LineIndex+1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	LineIndex5 is LineIndex-1,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	(
		P1 =< 6, P1 >= 1,
		P2 =< 6, P2 >= 1,
		P3 =< 6, P3 >= 1,
		P4 =< 6, P4 >= 1,
		P5 =< 6, P5 >= 1,
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_plus(Board,LineIndex,NextColumnIndex,1,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_plus(Board,LineIndex,NextColumnIndex,1,Points).
	
verify_plus(_,8,_,2,_).
verify_plus(Board,LineIndex,8,2,Points):-
	NewLineIndex is LineIndex+1,
	verify_plus(Board,NewLineIndex,2,2,Points).
verify_plus(Board,LineIndex,ColumnIndex,2,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex,ColumnIndex2,P2),
	ColumnIndex3 is ColumnIndex+1,
	get_value(Board,LineIndex,ColumnIndex3,P3),
	LineIndex4 is LineIndex+1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	LineIndex5 is LineIndex-1,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	(
		((P1 =< 3, P1 >= 1);(P1 =< 9, P1 >= 7)),
		((P2 =< 3, P2 >= 1);(P2 =< 9, P2 >= 7)),
		((P3 =< 3, P3 >= 1);(P3 =< 9, P3 >= 7)),
		((P4 =< 3, P4 >= 1);(P4 =< 9, P4 >= 7)),
		((P5 =< 3, P5 >= 1);(P5 =< 9, P5 >= 7)),
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_plus(Board,LineIndex,NextColumnIndex,2,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_plus(Board,LineIndex,NextColumnIndex,2,Points).
verify_plus(_,_,_,_,_).

	
verify_x(_,8,_,1,_).
verify_x(Board,LineIndex,8,1,Points):-
	NewLineIndex is LineIndex+1,
	verify_x(Board,NewLineIndex,2,1,Points).
verify_x(Board,LineIndex,ColumnIndex,1,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex-1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+1,
	ColumnIndex3 is ColumnIndex-1,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex+1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-1,
	ColumnIndex5 is ColumnIndex+1,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		P1 =< 6, P1 >= 1,
		P2 =< 6, P2 >= 1,
		P3 =< 6, P3 >= 1,
		P4 =< 6, P4 >= 1,
		P5 =< 6, P5 >= 1,
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_x(Board,LineIndex,NextColumnIndex,1,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_x(Board,LineIndex,NextColumnIndex,1,Points).
	
verify_x(_,8,_,2,_).
verify_x(Board,LineIndex,8,2,Points):-
	NewLineIndex is LineIndex+1,
	verify_x(Board,NewLineIndex,2,2,Points).
verify_x(Board,LineIndex,ColumnIndex,2,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex-1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+1,
	ColumnIndex3 is ColumnIndex-1,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex+1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-1,
	ColumnIndex5 is ColumnIndex+1,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		((P1 =< 3, P1 >= 1);(P1 =< 9, P1 >= 7)),
		((P2 =< 3, P2 >= 1);(P2 =< 9, P2 >= 7)),
		((P3 =< 3, P3 >= 1);(P3 =< 9, P3 >= 7)),
		((P4 =< 3, P4 >= 1);(P4 =< 9, P4 >= 7)),
		((P5 =< 3, P5 >= 1);(P5 =< 9, P5 >= 7)),
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_x(Board,LineIndex,NextColumnIndex,2,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_x(Board,LineIndex,NextColumnIndex,2,Points).
verify_x(_,_,_,_,_).

verify_diagonal_line_decrescent(_,7,_,1,_).
verify_diagonal_line_decrescent(Board,LineIndex,7,1,Points):-
	NewLineIndex is LineIndex+1,
	verify_diagonal_line_decrescent(Board,NewLineIndex,3,1,Points).
verify_diagonal_line_decrescent(Board,LineIndex,ColumnIndex,1,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex+1,
	ColumnIndex2 is ColumnIndex+1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+2,
	ColumnIndex3 is ColumnIndex+2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex-1,
	ColumnIndex4 is ColumnIndex-1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-2,
	ColumnIndex5 is ColumnIndex-2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		P1 =< 6, P1 >= 1,
		P2 =< 6, P2 >= 1,
		P3 =< 6, P3 >= 1,
		P4 =< 6, P4 >= 1,
		P5 =< 6, P5 >= 1,
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_diagonal_line_decrescent(Board,LineIndex,NextColumnIndex,1,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_diagonal_line_decrescent(Board,LineIndex,NextColumnIndex,1,Points).
	
verify_diagonal_line_decrescent(_,7,_,2,_).
verify_diagonal_line_decrescent(Board,LineIndex,7,2,Points):-
	NewLineIndex is LineIndex+1,
	verify_diagonal_line_decrescent(Board,NewLineIndex,3,2,Points).
verify_diagonal_line_decrescent(Board,LineIndex,ColumnIndex,2,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex+1,
	ColumnIndex2 is ColumnIndex+1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+2,
	ColumnIndex3 is ColumnIndex+2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex-1,
	ColumnIndex4 is ColumnIndex-1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-2,
	ColumnIndex5 is ColumnIndex-2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		((P1 =< 3, P1 >= 1);(P1 =< 9, P1 >= 7)),
		((P2 =< 3, P2 >= 1);(P2 =< 9, P2 >= 7)),
		((P3 =< 3, P3 >= 1);(P3 =< 9, P3 >= 7)),
		((P4 =< 3, P4 >= 1);(P4 =< 9, P4 >= 7)),
		((P5 =< 3, P5 >= 1);(P5 =< 9, P5 >= 7)),
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_diagonal_line_decrescent(Board,LineIndex,NextColumnIndex,2,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_diagonal_line_decrescent(Board,LineIndex,NextColumnIndex,2,Points).
verify_diagonal_line_decrescent(_,_,_,_,_).
	
verify_diagonal_line_crescent(_,7,_,1,_).
verify_diagonal_line_crescent(Board,LineIndex,7,1,Points):-
	NewLineIndex is LineIndex+1,
	verify_diagonal_line_crescent(Board,NewLineIndex,3,1,Points).
verify_diagonal_line_crescent(Board,LineIndex,ColumnIndex,1,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex+1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+2,
	ColumnIndex3 is ColumnIndex-2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex-1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-2,
	ColumnIndex5 is ColumnIndex+2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		P1 =< 6, P1 >= 1,
		P2 =< 6, P2 >= 1,
		P3 =< 6, P3 >= 1,
		P4 =< 6, P4 >= 1,
		P5 =< 6, P5 >= 1,
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_diagonal_line_crescent(Board,LineIndex,NextColumnIndex,1,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_diagonal_line_crescent(Board,LineIndex,NextColumnIndex,1,Points).
	
verify_diagonal_line_crescent(_,7,_,2,_).
verify_diagonal_line_crescent(Board,LineIndex,7,2,Points):-
	NewLineIndex is LineIndex+1,
	verify_diagonal_line_crescent(Board,NewLineIndex,3,2,Points).
verify_diagonal_line_crescent(Board,LineIndex,ColumnIndex,2,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex+1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+2,
	ColumnIndex3 is ColumnIndex-2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex-1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-2,
	ColumnIndex5 is ColumnIndex+2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		((P1 =< 3, P1 >= 1);(P1 =< 9, P1 >= 7)),
		((P2 =< 3, P2 >= 1);(P2 =< 9, P2 >= 7)),
		((P3 =< 3, P3 >= 1);(P3 =< 9, P3 >= 7)),
		((P4 =< 3, P4 >= 1);(P4 =< 9, P4 >= 7)),
		((P5 =< 3, P5 >= 1);(P5 =< 9, P5 >= 7)),
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_diagonal_line_crescent(Board,LineIndex,NextColumnIndex,2,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_diagonal_line_crescent(Board,LineIndex,NextColumnIndex,2,Points).
verify_diagonal_line_crescent(_,_,_,_,_).
	
verify_horizontal_line(_,9,_,1,_).
verify_horizontal_line(Board,LineIndex,9,1,Points):-
	NewLineIndex is LineIndex+1,
	verify_horizontal_line(Board,NewLineIndex,3,1,Points).
verify_horizontal_line(Board,LineIndex,ColumnIndex,1,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex,ColumnIndex2,P2),
	ColumnIndex3 is ColumnIndex-2,
	get_value(Board,LineIndex,ColumnIndex3,P3),
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex,ColumnIndex4,P4),
	ColumnIndex5 is ColumnIndex+2,
	get_value(Board,LineIndex,ColumnIndex5,P5),
	(
		P1 =< 6, P1 >= 1,
		P2 =< 6, P2 >= 1,
		P3 =< 6, P3 >= 1,
		P4 =< 6, P4 >= 1,
		P5 =< 6, P5 >= 1,
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_horizontal_line(Board,LineIndex,NextColumnIndex,1,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_horizontal_line(Board,LineIndex,NextColumnIndex,1,Points).
	
verify_horizontal_line(_,9,_,2,_).
verify_horizontal_line(Board,LineIndex,9,2,Points):-
	NewLineIndex is LineIndex+1,
	verify_horizontal_line(Board,NewLineIndex,3,2,Points).
verify_horizontal_line(Board,LineIndex,ColumnIndex,2,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex,ColumnIndex2,P2),
	ColumnIndex3 is ColumnIndex-2,
	get_value(Board,LineIndex,ColumnIndex3,P3),
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex,ColumnIndex4,P4),
	ColumnIndex5 is ColumnIndex+2,
	get_value(Board,LineIndex,ColumnIndex5,P5),
	(
		((P1 =< 3, P1 >= 1);(P1 =< 9, P1 >= 7)),
		((P2 =< 3, P2 >= 1);(P2 =< 9, P2 >= 7)),
		((P3 =< 3, P3 >= 1);(P3 =< 9, P3 >= 7)),
		((P4 =< 3, P4 >= 1);(P4 =< 9, P4 >= 7)),
		((P5 =< 3, P5 >= 1);(P5 =< 9, P5 >= 7)),
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex,ColumnIndex2]],Points1),
		append(Points1,[[LineIndex,ColumnIndex3]],Points2),
		append(Points2,[[LineIndex,ColumnIndex4]],Points3),
		append(Points3,[[LineIndex,ColumnIndex5]],NewPoints),
		NextColumnIndex is ColumnIndex+1,
		verify_horizontal_line(Board,LineIndex,NextColumnIndex,2,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_horizontal_line(Board,LineIndex,NextColumnIndex,2,Points).
verify_horizontal_line(_,_,_,_,_).
	
verify_vertical_line(_,_,9,1,_).
verify_vertical_line(Board,9,ColumnIndex,1,Points):-
	NewColumnIndex is ColumnIndex+1,
	verify_vertical_line(Board,3,NewColumnIndex,1,Points).
verify_vertical_line(Board,LineIndex,ColumnIndex,1,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex-1,
	get_value(Board,LineIndex2,ColumnIndex,P2),
	LineIndex3 is LineIndex-2,
	get_value(Board,LineIndex3,ColumnIndex,P3),
	LineIndex4 is LineIndex+1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	LineIndex5 is LineIndex+2,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	(
		P1 =< 6, P1 >= 1,
		P2 =< 6, P2 >= 1,
		P3 =< 6, P3 >= 1,
		P4 =< 6, P4 >= 1,
		P5 =< 6, P5 >= 1,
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex]],NewPoints),
		NextLineIndex is LineIndex+1,
		verify_vertical_line(Board,NextLineIndex,ColumnIndex,1,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextLineIndex is LineIndex+1,
	verify_vertical_line(Board,NextLineIndex,ColumnIndex,1,Points).
	
verify_vertical_line(_,_,9,2,_).
verify_vertical_line(Board,9,ColumnIndex,2,Points):-
	NewColumnIndex is ColumnIndex+1,
	verify_vertical_line(Board,3,NewColumnIndex,2,Points).
verify_vertical_line(Board,LineIndex,ColumnIndex,2,Points):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex-1,
	get_value(Board,LineIndex2,ColumnIndex,P2),
	LineIndex3 is LineIndex-2,
	get_value(Board,LineIndex3,ColumnIndex,P3),
	LineIndex4 is LineIndex+1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	LineIndex5 is LineIndex+2,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	(
		((P1 =< 3, P1 >= 1);(P1 =< 9, P1 >= 7)),
		((P2 =< 3, P2 >= 1);(P2 =< 9, P2 >= 7)),
		((P3 =< 3, P3 >= 1);(P3 =< 9, P3 >= 7)),
		((P4 =< 3, P4 >= 1);(P4 =< 9, P4 >= 7)),
		((P5 =< 3, P5 >= 1);(P5 =< 9, P5 >= 7)),
		write('Foi detetado um padrao, que resulta nos pontos: '),nl,
		append([[LineIndex,ColumnIndex]],[[LineIndex2,ColumnIndex]],Points1),
		append(Points1,[[LineIndex3,ColumnIndex]],Points2),
		append(Points2,[[LineIndex4,ColumnIndex]],Points3),
		append(Points3,[[LineIndex5,ColumnIndex]],NewPoints),
		NextLineIndex is LineIndex+1,
		verify_vertical_line(Board,NextLineIndex,ColumnIndex,2,NewPoints2),
		append(NewPoints,NewPoints2,Points)
	);
	NextLineIndex is LineIndex+1,
	verify_vertical_line(Board,NextLineIndex,ColumnIndex,2,Points).
verify_vertical_line(_,_,_,_,_).

count(_, [], 0) :- !. 

count(X, [X|T], N) :- 
    count(X, T, N1), 
    N is N1 + 1.     

count(X, [Y|T], N) :- 
    X \= Y,          
    count(X, T, N). 
	
count_points(_,9,0,0).
	
count_points(Board,LineIndex,Points1,Points2):-
	get_line(LineIndex,Board,Line),
	count(2,Line,Points1_2),
	count(5,Line,Points1_5),
	count(8,Line,Points1_8),
	count(10,Line,Points1_10),
	count(3,Line,Points2_3),
	count(6,Line,Points2_6),
	count(9,Line,Points2_9),
	count(11,Line,Points2_11),
	Points1_line is Points1_2+Points1_5+Points1_8+Points1_10,
	Points2_line is Points2_3+Points2_6+Points2_9+Points2_11,
	NextLineIndex is LineIndex+1,
	count_points(Board,NextLineIndex,NewPoints1,NewPoints2),
	Points1 is NewPoints1+Points1_line,
	Points2 is NewPoints2+Points2_line.
	
count_jokers(_,9,0).
count_jokers(Board,LineIndex,Jokers):-
	get_line(LineIndex,Board,Line),
	count(1,Line,Jokers_1),
	count(2,Line,Jokers_2),
	count(3,Line,Jokers_3),
	LineJokers is Jokers_1+Jokers_2+Jokers_3,
	NextLineIndex is LineIndex+1,
	count_jokers(Board,NextLineIndex,NewJokers),
	Jokers is NewJokers+LineJokers.
	
convert_joker(0,1).
convert_joker(10,2).
convert_joker(11,3).	

ask_jokers(X,_,_,6,X).
ask_jokers(Board,Player,Name,NJokers,NewBoard):-
	NJokers < 5,
	repeat,
	write(Name), write(', introduz a posicao onde pretende colocar o joker.'),nl,
	read(Pos),
	valid_input(Pos),
	argvl(Pos,X,Y),
	get_value(Board,X,Y,Value),
	valid_position(Value),
	convert_joker(Value,NewValue),
	get_new_board(Board,Pos,NewValue,NextBoard,Player),
	NewJokers is NJokers+1,
	ask_jokers(NextBoard,Player,Name,NewJokers,NewBoard).
	
ask_jokers(X,_,_,_,X).
	
write_points(Points1,Points2,Name1,Name2):-
	nl,write(Name1),write(': '), write(Points1),
	write(' ponto(s).                '),write(Name2),write(': '), write(Points2), write(' ponto(s).'),nl.

verify_endgame(Board,Player,Name,Points1,Points2,MaxPoints,MaxPieces):-
	verify_endgame_by_points(Name,Points1,Points2,MaxPoints);
	verify_endgame_by_pieces(Board,Name,Player,MaxPieces,Points1,Points2);
	verify_endgame_by_joker_pattern(Board,Name).

verify_endgame_by_points(Name,Points1,Points2,MaxPoints):-
	Points1 >= MaxPoints, endgame_by_points(Name,Points1);
	Points2 >= MaxPoints, endgame_by_points(Name,Points2).

endgame_by_points(Player,Points):-
	nl,nl,
	write('Parabens '), write(Player), write(' ganhou o jogo com '), write(Points), write(' pontos!'),
	nl,nl,exit_game,
	play.
	
verify_endgame_by_pieces(Board,Name,Player,MaxPieces,Points1,Points2):-
	count_pieces(Board,1,Player,Pieces),
	write(Name), write(' tem '), write(Pieces), write(' peca(s) colocada(s).'),
	nl,
	Pieces >= MaxPieces,
	(
		Points1 > Points2, endgame_by_pieces(Name,Points1);
		Points1 < Points2, endgame_by_pieces(Name,Points2);
		Points1 = Points2, endgame_tie
	).

endgame_tie:-
	nl,nl,
	write('O jogo terminou empatado devido as pecas terem terminado, e os jogadores terem os mesmos pontos!'),
	nl,nl,exit_game,
	play.
	
count_pieces(_,9,1,0).
count_pieces(Board,LineIndex,1,Pieces):-
	get_line(LineIndex,Board,Line),
	count(4,Line,Pieces_4),
	count(5,Line,Pieces_5),
	count(6,Line,Pieces_6),
	Pieces_line is Pieces_4+Pieces_5+Pieces_6,
	NextLineIndex is LineIndex+1,
	count_pieces(Board,NextLineIndex,1,NewPieces),
	Pieces is NewPieces+Pieces_line.
	
count_pieces(_,9,2,0).
count_pieces(Board,LineIndex,2,Pieces):-
	get_line(LineIndex,Board,Line),
	count(7,Line,Pieces_7),
	count(8,Line,Pieces_8),
	count(9,Line,Pieces_9),
	Pieces_line is Pieces_7+Pieces_8+Pieces_9,
	NextLineIndex is LineIndex+1,
	count_pieces(Board,NextLineIndex,2,NewPieces),
	Pieces is NewPieces+Pieces_line.
	
endgame_by_pieces(Player,Points):-
	nl,nl,
	write('Parabens '), write(Player), write(' ganhou o jogo com '), write(Points), write(' pontos, devido a terem acabado as pecas!'),
	nl,nl,exit_game,
	play.
	
verify_endgame_by_joker_pattern(Board,Name):-
	verify_joker_horizontal_line(Board,1,Name),
	verify_joker_vertical_line(Board,3,1,Name),
	verify_joker_diagonal_line_crescent(Board,3,3,Name),
	verify_joker_diagonal_line_decrescent(Board,3,3,Name),
	verify_joker_x(Board,2,2,Name),
	verify_joker_plus(Board,2,2,Name).
	
	
verify_joker_horizontal_line(_,9,_).
verify_joker_horizontal_line(Board,LineIndex,Name):-
	get_line(LineIndex,Board,Line),
	verify_joker_horizontal_line_in_line(Line,3,Name),
	NewLineIndex is LineIndex+1,
	verify_joker_horizontal_line(Board,NewLineIndex,Name).
	
verify_joker_horizontal_line_in_line(_,9,_).
verify_joker_horizontal_line_in_line(Line,ColumnIndex,Name):-
	get_value_line(Line,ColumnIndex,P1),
	ColumnIndex2 is ColumnIndex-1,
	get_value_line(Line,ColumnIndex2,P2),
	ColumnIndex3 is ColumnIndex-2,
	get_value_line(Line,ColumnIndex3,P3),
	ColumnIndex4 is ColumnIndex+1,
	get_value_line(Line,ColumnIndex4,P4),
	ColumnIndex5 is ColumnIndex+2,
	get_value_line(Line,ColumnIndex5,P5),
	(
		P1 =< 3, P1 >= 1,
		P2 =< 3, P2 >= 1,
		P3 =< 3, P3 >= 1,
		P4 =< 3, P4 >= 1,
		P5 =< 3, P5 >= 1,
		endgame_by_joker_pattern(Name)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_joker_horizontal_line_in_line(Line,NextColumnIndex,Name).
	
verify_joker_vertical_line(_,_,9,_).
verify_joker_vertical_line(Board,9,ColumnIndex,Name):-
	NewColumnIndex is ColumnIndex+1,
	verify_joker_vertical_line(Board,3,NewColumnIndex,Name).
verify_joker_vertical_line(Board,LineIndex,ColumnIndex,Name):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex-1,
	get_value(Board,LineIndex2,ColumnIndex,P2),
	LineIndex3 is LineIndex-2,
	get_value(Board,LineIndex3,ColumnIndex,P3),
	LineIndex4 is LineIndex+1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	LineIndex5 is LineIndex+2,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	(
		P1 =< 3, P1 >= 1,
		P2 =< 3, P2 >= 1,
		P3 =< 3, P3 >= 1,
		P4 =< 3, P4 >= 1,
		P5 =< 3, P5 >= 1,
		endgame_by_joker_pattern(Name)
	);
	NextLineIndex is LineIndex+1,
	verify_joker_vertical_line(Board,NextLineIndex,ColumnIndex,Name).
	
verify_joker_diagonal_line_crescent(_,7,_,_).
verify_joker_diagonal_line_crescent(Board,LineIndex,7,Name):-
	NewLineIndex is LineIndex+1,
	verify_joker_diagonal_line_crescent(Board,NewLineIndex,3,Name).
verify_joker_diagonal_line_crescent(Board,LineIndex,ColumnIndex,Name):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex+1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+2,
	ColumnIndex3 is ColumnIndex-2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex-1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-2,
	ColumnIndex5 is ColumnIndex+2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		P1 =< 3, P1 >= 1,
		P2 =< 3, P2 >= 1,
		P3 =< 3, P3 >= 1,
		P4 =< 3, P4 >= 1,
		P5 =< 3, P5 >= 1,
		endgame_by_joker_pattern(Name)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_joker_diagonal_line_crescent(Board,LineIndex,NextColumnIndex,Name).
	
verify_joker_diagonal_line_decrescent(_,7,_,_).
verify_joker_diagonal_line_decrescent(Board,LineIndex,7,Name):-
	NewLineIndex is LineIndex+1,
	verify_joker_diagonal_line_decrescent(Board,NewLineIndex,3,Name).
verify_joker_diagonal_line_decrescent(Board,LineIndex,ColumnIndex,Name):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex-1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex-2,
	ColumnIndex3 is ColumnIndex-2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex+1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex+2,
	ColumnIndex5 is ColumnIndex+2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		P1 =< 3, P1 >= 1,
		P2 =< 3, P2 >= 1,
		P3 =< 3, P3 >= 1,
		P4 =< 3, P4 >= 1,
		P5 =< 3, P5 >= 1,
		endgame_by_joker_pattern(Name)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_joker_diagonal_line_decrescent(Board,LineIndex,NextColumnIndex,Name).
	
verify_joker_x(_,8,_,_).
verify_joker_x(Board,LineIndex,8,Name):-
	NewLineIndex is LineIndex+1,
	verify_joker_x(Board,NewLineIndex,2,Name).
verify_joker_x(Board,LineIndex,ColumnIndex,Name):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	LineIndex2 is LineIndex-1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	LineIndex3 is LineIndex+1,
	ColumnIndex3 is ColumnIndex-1,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	LineIndex4 is LineIndex+1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	LineIndex5 is LineIndex-1,
	ColumnIndex5 is ColumnIndex+1,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	(
		P1 =< 3, P1 >= 1,
		P2 =< 3, P2 >= 1,
		P3 =< 3, P3 >= 1,
		P4 =< 3, P4 >= 1,
		P5 =< 3, P5 >= 1,
		endgame_by_joker_pattern(Name)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_joker_x(Board,LineIndex,NextColumnIndex,Name).
	
verify_joker_plus(_,8,_,_).
verify_joker_plus(Board,LineIndex,8,Name):-
	NewLineIndex is LineIndex+1,
	verify_joker_plus(Board,NewLineIndex,2,Name).
verify_joker_plus(Board,LineIndex,ColumnIndex,Name):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex,ColumnIndex2,P2),
	ColumnIndex3 is ColumnIndex+1,
	get_value(Board,LineIndex,ColumnIndex3,P3),
	LineIndex4 is LineIndex+1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	LineIndex5 is LineIndex-1,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	(
		P1 =< 3, P1 >= 1,
		P2 =< 3, P2 >= 1,
		P3 =< 3, P3 >= 1,
		P4 =< 3, P4 >= 1,
		P5 =< 3, P5 >= 1,
		endgame_by_joker_pattern(Name)
	);
	NextColumnIndex is ColumnIndex+1,
	verify_joker_plus(Board,LineIndex,NextColumnIndex,Name).
	
endgame_by_joker_pattern(Player):-
	nl,nl,
	write('Parabens '), write(Player), write(' ganhou o jogo por ter feito um padrao apenas com jokers! Extraordinario!'),
	nl,nl,exit_game,
	play.

human_vs_computer_play(Board,Player,MaxPoints,MaxPieces,Dificulty,Name):-
	nl,
	repeat,
	write(Name),write(', e a tua vez de jogar!'),nl,
	write('Qual a posicao da nova jogada (linha e coluna)? '), nl,
	read(Pos),
	valid_input(Pos),
	argvl(Pos,Line,Column),
	get_value(Board,Line,Column,Value),
	valid_position(Value),
	get_player_arg(Player,Value,A),
	get_new_board(Board,Pos,A,NBoard,Player),
	printt(NBoard),
	verify_patterns(NBoard,Player,Points),
	print_points(Points),
	update_points(NBoard,Points,Player,UBoard),
	count_jokers(UBoard,1,Jokers),
	ask_jokers(UBoard,Player,Name,Jokers,FinalBoard),
	printt(FinalBoard),
	count_points(FinalBoard,1,Points1,Points2),
	send(@scrore1,string(Points1)),
	send(@scrore2,string(Points2)),
	write_points(Points1,Points2,Name,'Computador'),
	verify_endgame(FinalBoard,Player,Name,Points1,Points2,MaxPoints,MaxPieces),
	human_vs_computer_play_automatic(FinalBoard,2,MaxPoints,MaxPieces,Dificulty,Name).
	
human_vs_computer_play_automatic(Board,Player,MaxPoints,MaxPieces,1,Name):-
	nl, write('Computador a jogar'),nl,nl,
	verify_semi_patterns(Board,Player,Position),
	argvl(Position,Line,Column),
	get_value(Board,Line,Column,Value),
	get_player_arg(Player,Value,A),
	get_new_board(Board,Position,A,NBoard,Player),
	printt(NBoard),
	verify_patterns(NBoard,Player,Points),
	print_points(Points),
	update_points(NBoard,Points,Player,UBoard),
	count_jokers(UBoard,1,Jokers),
	ask_jokers_computer(UBoard,Jokers,FinalBoard),
	printt(FinalBoard),
	count_points(FinalBoard,1,Points1,Points2),
	send(@scrore1,string(Points1)),
	send(@scrore2,string(Points2)),
	write_points(Points1,Points2,Name,'Computador'),
	verify_endgame(FinalBoard,Player,'Computador',Points1,Points2,MaxPoints,MaxPieces),
	human_vs_computer_play(FinalBoard,1,MaxPoints,MaxPieces,1,Name).
	
human_vs_computer_play_automatic(Board,Player,MaxPoints,MaxPieces,2,Name):-
	(
		verify_semi_patterns_block(Board,1,Position);
		verify_semi_patterns(Board,Player,Position)
	),
	nl, write('Computador a jogar'),nl,nl,
	argvl(Position,Line,Column),
	get_value(Board,Line,Column,Value),
	get_player_arg(Player,Value,A),
	get_new_board(Board,Position,A,NBoard,Player),
	printt(NBoard),
	verify_patterns(NBoard,Player,Points),
	print_points(Points),
	update_points(NBoard,Points,Player,UBoard),
	count_jokers(UBoard,1,Jokers),
	ask_jokers_computer(UBoard,Jokers,FinalBoard),
	printt(FinalBoard),
	count_points(FinalBoard,1,Points1,Points2),
	send(@scrore1,string(Points1)),
	send(@scrore2,string(Points2)),
	write_points(Points1,Points2,Name,'Computador'),
	verify_endgame(FinalBoard,Player,'Computador',Points1,Points2,MaxPoints,MaxPieces),
	human_vs_computer_play(FinalBoard,1,MaxPoints,MaxPieces,2,Name).
	
verify_semi_patterns(Board,Player,Position):-
	get_semi_plus(Board,2,2,Player,PointsPlus,ListSumPlus,EmptySpacesPlus),
	select_best(PointsPlus,ListSumPlus,BestPosPlus,BestSumPlus,IndexPlus),
	get_semi_x(Board,2,2,Player,PointsX,ListSumX,EmptySpacesX),
	select_best(PointsX,ListSumX,BestPosX,BestSumX,IndexX),
	get_semi_diagonal_line_decrescent(Board,3,3,Player,PointsDiagLineDec,ListSumDiagLineDec,EmptySpacesDiagLineDec),
	select_best(PointsDiagLineDec,ListSumDiagLineDec,BestPosDiagLineDec,BestSumDiagLineDec,IndexDiagLineDec),
	get_semi_diagonal_line_crescent(Board,3,3,Player,PointsDiagLineCrs,ListSumDiagLineCrs,EmptySpacesDiagLineCrs),
	select_best(PointsDiagLineCrs,ListSumDiagLineCrs,BestPosDiagLineCrs,BestSumDiagLineCrs,IndexDiagLineCrs),
	get_semi_horizontal_line(Board,1,3,Player,PointsHorizontal,ListSumHorizontal,EmptySpacesHorizontal),
	select_best(PointsHorizontal,ListSumHorizontal,BestPosHorizontal,BestSumHorizontal,IndexHorizontal),
	get_semi_vertical_line(Board,3,1,Player,PointsVertical,ListSumVertical,EmptySpacesVertical),
	select_best(PointsVertical,ListSumVertical,BestPosVertical,BestSumVertical,IndexVertical),

	append([BestPosPlus],[BestPosX],BestPos1),
	append(BestPos1,[BestPosDiagLineDec],BestPos2),
	append(BestPos2,[BestPosDiagLineCrs],BestPos3),
	append(BestPos3,[BestPosHorizontal],BestPos4),
	append(BestPos4,[BestPosVertical],BestPos5),
	
	append([BestSumPlus],[BestSumX],BestSum1),
	append(BestSum1,[BestSumDiagLineDec],BestSum2),
	append(BestSum2,[BestSumDiagLineCrs],BestSum3),
	append(BestSum3,[BestSumHorizontal],BestSum4),
	append(BestSum4,[BestSumVertical],BestSum5),
	
	nth1(IndexPlus,EmptySpacesPlus,FinalEmptySpacesPlus),
	nth1(IndexX,EmptySpacesX,FinalEmptySpacesX),
	nth1(IndexDiagLineDec,EmptySpacesDiagLineDec,FinalEmptySpacesDiagLineDec),
	nth1(IndexDiagLineCrs,EmptySpacesDiagLineCrs,FinalEmptySpacesDiagLineCrs),
	nth1(IndexHorizontal,EmptySpacesHorizontal,FinalEmptySpacesHorizontal),
	nth1(IndexVertical,EmptySpacesVertical,FinalEmptySpacesVertical),
	
	append([FinalEmptySpacesPlus],[FinalEmptySpacesX],EmptySpaces1),
	append(EmptySpaces1,[FinalEmptySpacesDiagLineDec],EmptySpaces2),
	append(EmptySpaces2,[FinalEmptySpacesDiagLineCrs],EmptySpaces3),
	append(EmptySpaces3,[FinalEmptySpacesHorizontal],EmptySpaces4),
	append(EmptySpaces4,[FinalEmptySpacesVertical],FinalEmptySpacesSet),
	
	select_best(BestPos5,BestSum5,_,_,IndexFinal),
	nth1(IndexFinal,FinalEmptySpacesSet,FinalEmptySpaces),
	nth1(1,FinalEmptySpaces,FinalPosition),
	nth1(1,FinalPosition,X),
	nth1(2,FinalPosition,Y),
	Position is X*10+Y.
	
get_semi_plus(_,8,_,_,[],[],[]).
get_semi_plus(Board,LineIndex,8,Player,Points,ListSum,EmptySpaces):-
	NewLineIndex is LineIndex+1,
	get_semi_plus(Board,NewLineIndex,2,Player,Points,ListSum,EmptySpaces).
get_semi_plus(Board,LineIndex,ColumnIndex,Player,Points,ListSum,EmptySpaces):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	append_empty_spaces(P1,LineIndex,ColumnIndex,[],EmptySpaces1),
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex,ColumnIndex2,P2),
	append_empty_spaces(P2,LineIndex,ColumnIndex2,EmptySpaces1,EmptySpaces2),
	ColumnIndex3 is ColumnIndex+1,
	get_value(Board,LineIndex,ColumnIndex3,P3),
	append_empty_spaces(P3,LineIndex,ColumnIndex3,EmptySpaces2,EmptySpaces3),
	LineIndex4 is LineIndex+1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	append_empty_spaces(P4,LineIndex4,ColumnIndex,EmptySpaces3,EmptySpaces4),
	LineIndex5 is LineIndex-1,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	append_empty_spaces(P5,LineIndex5,ColumnIndex,EmptySpaces4,FinalEmptySpaces),
	verify_piece_of_player(P1,Player,0,Sum),
	verify_piece_of_player(P2,Player,Sum,NewSum1),
	verify_piece_of_player(P3,Player,NewSum1,NewSum2),
	verify_piece_of_player(P4,Player,NewSum2,NewSum3),
	verify_piece_of_player(P5,Player,NewSum3,FinalSum),
	append([[LineIndex,ColumnIndex]],[],NewPoints),
	NextColumnIndex is ColumnIndex+1,
	get_semi_plus(Board,LineIndex,NextColumnIndex,Player,NewPoints2,NewListSum,NewEmptySpaces),
	append(NewPoints,NewPoints2,Points),
	append([FinalSum],NewListSum,ListSum),
	append([FinalEmptySpaces],NewEmptySpaces,EmptySpaces).
get_semi_plus(_,_,_,_,[],[],[]).

get_semi_x(_,8,_,_,[],[],[]).
get_semi_x(Board,LineIndex,8,Player,Points,ListSum,EmptySpaces):-
	NewLineIndex is LineIndex+1,
	get_semi_x(Board,NewLineIndex,2,Player,Points,ListSum,EmptySpaces).
get_semi_x(Board,LineIndex,ColumnIndex,Player,Points,ListSum,EmptySpaces):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	append_empty_spaces(P1,LineIndex,ColumnIndex,[],EmptySpaces1),
	LineIndex2 is LineIndex-1,
	ColumnIndex2 is ColumnIndex-1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	append_empty_spaces(P2,LineIndex2,ColumnIndex2,EmptySpaces1,EmptySpaces2),
	LineIndex3 is LineIndex+1,
	ColumnIndex3 is ColumnIndex-1,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	append_empty_spaces(P3,LineIndex3,ColumnIndex3,EmptySpaces2,EmptySpaces3),
	LineIndex4 is LineIndex+1,
	ColumnIndex4 is ColumnIndex+1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	append_empty_spaces(P4,LineIndex4,ColumnIndex4,EmptySpaces3,EmptySpaces4),
	LineIndex5 is LineIndex-1,
	ColumnIndex5 is ColumnIndex+1,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	append_empty_spaces(P5,LineIndex5,ColumnIndex5,EmptySpaces4,FinalEmptySpaces),
	verify_piece_of_player(P1,Player,0,Sum),
	verify_piece_of_player(P2,Player,Sum,NewSum1),
	verify_piece_of_player(P3,Player,NewSum1,NewSum2),
	verify_piece_of_player(P4,Player,NewSum2,NewSum3),
	verify_piece_of_player(P5,Player,NewSum3,FinalSum),
	append([[LineIndex,ColumnIndex]],[],NewPoints),
	NextColumnIndex is ColumnIndex+1,
	get_semi_x(Board,LineIndex,NextColumnIndex,Player,NewPoints2,NewListSum,NewEmptySpaces),
	append(NewPoints,NewPoints2,Points),
	append([FinalSum],NewListSum,ListSum),
	append([FinalEmptySpaces],NewEmptySpaces,EmptySpaces).
get_semi_x(_,_,_,_,[],[],[]).

get_semi_diagonal_line_decrescent(_,7,_,_,[],[],[]).
get_semi_diagonal_line_decrescent(Board,LineIndex,7,Player,Points,ListSum,EmptySpaces):-
	NewLineIndex is LineIndex+1,
	get_semi_diagonal_line_decrescent(Board,NewLineIndex,3,Player,Points,ListSum,EmptySpaces).
get_semi_diagonal_line_decrescent(Board,LineIndex,ColumnIndex,Player,Points,ListSum,EmptySpaces):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	append_empty_spaces(P1,LineIndex,ColumnIndex,[],EmptySpaces1),
	LineIndex2 is LineIndex+1,
	ColumnIndex2 is ColumnIndex+1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	append_empty_spaces(P2,LineIndex2,ColumnIndex2,EmptySpaces1,EmptySpaces2),
	LineIndex3 is LineIndex+2,
	ColumnIndex3 is ColumnIndex+2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	append_empty_spaces(P3,LineIndex3,ColumnIndex3,EmptySpaces2,EmptySpaces3),
	LineIndex4 is LineIndex-1,
	ColumnIndex4 is ColumnIndex-1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	append_empty_spaces(P4,LineIndex4,ColumnIndex4,EmptySpaces3,EmptySpaces4),
	LineIndex5 is LineIndex-2,
	ColumnIndex5 is ColumnIndex-2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	append_empty_spaces(P5,LineIndex5,ColumnIndex5,EmptySpaces4,FinalEmptySpaces),
	verify_piece_of_player(P1,Player,0,Sum),
	verify_piece_of_player(P2,Player,Sum,NewSum1),
	verify_piece_of_player(P3,Player,NewSum1,NewSum2),
	verify_piece_of_player(P4,Player,NewSum2,NewSum3),
	verify_piece_of_player(P5,Player,NewSum3,FinalSum),
	append([[LineIndex,ColumnIndex]],[],NewPoints),
	NextColumnIndex is ColumnIndex+1,
	get_semi_diagonal_line_decrescent(Board,LineIndex,NextColumnIndex,Player,NewPoints2,NewListSum,NewEmptySpaces),
	append(NewPoints,NewPoints2,Points),
	append([FinalSum],NewListSum,ListSum),
	append([FinalEmptySpaces],NewEmptySpaces,EmptySpaces).
get_semi_diagonal_line_decrescent(_,_,_,_,[],[],[]).
	
get_semi_diagonal_line_crescent(_,7,_,_,[],[],[]).
get_semi_diagonal_line_crescent(Board,LineIndex,7,Player,Points,ListSum,EmptySpaces):-
	NewLineIndex is LineIndex+1,
	get_semi_diagonal_line_crescent(Board,NewLineIndex,3,Player,Points,ListSum,EmptySpaces).
get_semi_diagonal_line_crescent(Board,LineIndex,ColumnIndex,Player,Points,ListSum,EmptySpaces):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	append_empty_spaces(P1,LineIndex,ColumnIndex,[],EmptySpaces1),
	LineIndex2 is LineIndex-1,
	ColumnIndex2 is ColumnIndex+1,
	get_value(Board,LineIndex2,ColumnIndex2,P2),
	append_empty_spaces(P2,LineIndex2,ColumnIndex2,EmptySpaces1,EmptySpaces2),
	LineIndex3 is LineIndex-2,
	ColumnIndex3 is ColumnIndex+2,
	get_value(Board,LineIndex3,ColumnIndex3,P3),
	append_empty_spaces(P3,LineIndex3,ColumnIndex3,EmptySpaces2,EmptySpaces3),
	LineIndex4 is LineIndex+1,
	ColumnIndex4 is ColumnIndex-1,
	get_value(Board,LineIndex4,ColumnIndex4,P4),
	append_empty_spaces(P4,LineIndex4,ColumnIndex4,EmptySpaces3,EmptySpaces4),
	LineIndex5 is LineIndex+2,
	ColumnIndex5 is ColumnIndex-2,
	get_value(Board,LineIndex5,ColumnIndex5,P5),
	append_empty_spaces(P5,LineIndex5,ColumnIndex5,EmptySpaces4,FinalEmptySpaces),
	verify_piece_of_player(P1,Player,0,Sum),
	verify_piece_of_player(P2,Player,Sum,NewSum1),
	verify_piece_of_player(P3,Player,NewSum1,NewSum2),
	verify_piece_of_player(P4,Player,NewSum2,NewSum3),
	verify_piece_of_player(P5,Player,NewSum3,FinalSum),
	append([[LineIndex,ColumnIndex]],[],NewPoints),
	NextColumnIndex is ColumnIndex+1,
	get_semi_diagonal_line_crescent(Board,LineIndex,NextColumnIndex,Player,NewPoints2,NewListSum,NewEmptySpaces),
	append(NewPoints,NewPoints2,Points),
	append([FinalSum],NewListSum,ListSum),
	append([FinalEmptySpaces],NewEmptySpaces,EmptySpaces).
get_semi_diagonal_line_crescent(_,_,_,_,[],[],[]).
	
get_semi_horizontal_line(_,9,_,_,[],[],[]).
get_semi_horizontal_line(Board,LineIndex,7,Player,Points,ListSum,EmptySpaces):-
	NewLineIndex is LineIndex+1,
	get_semi_horizontal_line(Board,NewLineIndex,3,Player,Points,ListSum,EmptySpaces).
get_semi_horizontal_line(Board,LineIndex,ColumnIndex,Player,Points,ListSum,EmptySpaces):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	append_empty_spaces(P1,LineIndex,ColumnIndex,[],EmptySpaces1),
	ColumnIndex2 is ColumnIndex+1,
	get_value(Board,LineIndex,ColumnIndex2,P2),
	append_empty_spaces(P2,LineIndex,ColumnIndex2,EmptySpaces1,EmptySpaces2),
	ColumnIndex3 is ColumnIndex+2,
	get_value(Board,LineIndex,ColumnIndex3,P3),
	append_empty_spaces(P3,LineIndex,ColumnIndex3,EmptySpaces2,EmptySpaces3),
	ColumnIndex4 is ColumnIndex-1,
	get_value(Board,LineIndex,ColumnIndex4,P4),
	append_empty_spaces(P4,LineIndex,ColumnIndex4,EmptySpaces3,EmptySpaces4),
	ColumnIndex5 is ColumnIndex-2,
	get_value(Board,LineIndex,ColumnIndex5,P5),
	append_empty_spaces(P5,LineIndex,ColumnIndex5,EmptySpaces4,FinalEmptySpaces),
	verify_piece_of_player(P1,Player,0,Sum),
	verify_piece_of_player(P2,Player,Sum,NewSum1),
	verify_piece_of_player(P3,Player,NewSum1,NewSum2),
	verify_piece_of_player(P4,Player,NewSum2,NewSum3),
	verify_piece_of_player(P5,Player,NewSum3,FinalSum),
	append([[LineIndex,ColumnIndex]],[],NewPoints),
	NextColumnIndex is ColumnIndex+1,
	get_semi_horizontal_line(Board,LineIndex,NextColumnIndex,Player,NewPoints2,NewListSum,NewEmptySpaces),
	append(NewPoints,NewPoints2,Points),
	append([FinalSum],NewListSum,ListSum),
	append([FinalEmptySpaces],NewEmptySpaces,EmptySpaces).
get_semi_horizontal_line(_,_,_,_,[],[],[]).
	
get_semi_vertical_line(_,_,9,_,[],[],[]).
get_semi_vertical_line(Board,7,ColumnIndex,Player,Points,ListSum,EmptySpaces):-
	NewColumnIndex is ColumnIndex+1,
	get_semi_vertical_line(Board,3,NewColumnIndex,Player,Points,ListSum,EmptySpaces).
get_semi_vertical_line(Board,LineIndex,ColumnIndex,Player,Points,ListSum,EmptySpaces):-
	get_value(Board,LineIndex,ColumnIndex,P1),
	append_empty_spaces(P1,LineIndex,ColumnIndex,[],EmptySpaces1),
	LineIndex2 is LineIndex+1,
	get_value(Board,LineIndex2,ColumnIndex,P2),
	append_empty_spaces(P2,LineIndex2,ColumnIndex,EmptySpaces1,EmptySpaces2),
	LineIndex3 is LineIndex+2,
	get_value(Board,LineIndex3,ColumnIndex,P3),
	append_empty_spaces(P3,LineIndex3,ColumnIndex,EmptySpaces2,EmptySpaces3),
	LineIndex4 is LineIndex-1,
	get_value(Board,LineIndex4,ColumnIndex,P4),
	append_empty_spaces(P4,LineIndex4,ColumnIndex,EmptySpaces3,EmptySpaces4),
	LineIndex5 is LineIndex-2,
	get_value(Board,LineIndex5,ColumnIndex,P5),
	append_empty_spaces(P5,LineIndex5,ColumnIndex,EmptySpaces4,FinalEmptySpaces),
	verify_piece_of_player(P1,Player,0,Sum),
	verify_piece_of_player(P2,Player,Sum,NewSum1),
	verify_piece_of_player(P3,Player,NewSum1,NewSum2),
	verify_piece_of_player(P4,Player,NewSum2,NewSum3),
	verify_piece_of_player(P5,Player,NewSum3,FinalSum),
	append([[LineIndex,ColumnIndex]],[],NewPoints),
	NextLineIndex is LineIndex+1,
	get_semi_vertical_line(Board,NextLineIndex,ColumnIndex,Player,NewPoints2,NewListSum,NewEmptySpaces),
	append(NewPoints,NewPoints2,Points),
	append([FinalSum],NewListSum,ListSum),
	append([FinalEmptySpaces],NewEmptySpaces,EmptySpaces).
get_semi_vertical_line(_,_,_,_,[],[],[]).

verify_piece_of_player(P1,Player,CurrentSum,Sum):-
	validate_value(P1,Player,Priority), Sum is CurrentSum+Priority.

append_empty_spaces(Value,LineIndex,ColumnIndex,CurrentList,EmptySpaces):-
	valid_position(Value),
	append([[LineIndex,ColumnIndex]],CurrentList,EmptySpaces);
	append([],CurrentList,EmptySpaces).

validate_value(0,1,0).
validate_value(1,1,1).
validate_value(2,1,1).
validate_value(3,1,1).
validate_value(4,1,1).
validate_value(5,1,1).
validate_value(6,1,1).
validate_value(7,1,-10).
validate_value(8,1,-10).
validate_value(9,1,-10).
validate_value(10,1,0).
validate_value(11,1,0).
validate_value(0,2,0).
validate_value(1,2,1).
validate_value(2,2,1).
validate_value(3,2,1).
validate_value(4,2,-10).
validate_value(5,2,-10).
validate_value(6,2,-10).
validate_value(7,2,1).
validate_value(8,2,1).
validate_value(9,2,1).
validate_value(10,2,0).
validate_value(11,2,0).

max_list(L, M, I) :- 
	nth1(I, L, M), 
	\+ (member(E, L),
	E > M).

select_best(PointsPlus,ListSum,BestPos,BestSum,MaxValueIndex):-
	max_list(ListSum,BestSum),
	nth1(MaxValueIndex,ListSum,BestSum),
	nth1(MaxValueIndex,PointsPlus,BestPos).
	
ask_jokers_computer(X,6,X).	
ask_jokers_computer(Board,Jokers,FinalBoard):-
	Jokers < 5,
	repeat,
	random(1,8,LineIndex),
	random(1,8,ColumnIndex),
	get_value(Board,LineIndex,ColumnIndex,Value),
	valid_position(Value),
	convert_joker(Value,NewValue),
	Pos is LineIndex*10+ColumnIndex,
	convert_joker(Value,NewValue),
	get_new_board(Board,Pos,NewValue,NextBoard,_),
	NewJokers is Jokers+1,
	ask_jokers_computer(NextBoard,NewJokers,FinalBoard).
	
ask_jokers_computer(X,_,X).
	
verify_semi_patterns_block(Board,Player,Block):-
	get_semi_plus(Board,2,2,Player,PointsPlus,ListSumPlus,EmptySpacesPlus),
	select_best(PointsPlus,ListSumPlus,BestPosPlus,BestSumPlus,IndexPlus),
	get_semi_x(Board,2,2,Player,PointsX,ListSumX,EmptySpacesX),
	select_best(PointsX,ListSumX,BestPosX,BestSumX,IndexX),
	get_semi_diagonal_line_decrescent(Board,3,3,Player,PointsDiagLineDec,ListSumDiagLineDec,EmptySpacesDiagLineDec),
	select_best(PointsDiagLineDec,ListSumDiagLineDec,BestPosDiagLineDec,BestSumDiagLineDec,IndexDiagLineDec),
	get_semi_diagonal_line_crescent(Board,3,3,Player,PointsDiagLineCrs,ListSumDiagLineCrs,EmptySpacesDiagLineCrs),
	select_best(PointsDiagLineCrs,ListSumDiagLineCrs,BestPosDiagLineCrs,BestSumDiagLineCrs,IndexDiagLineCrs),
	get_semi_horizontal_line(Board,1,3,Player,PointsHorizontal,ListSumHorizontal,EmptySpacesHorizontal),
	select_best(PointsHorizontal,ListSumHorizontal,BestPosHorizontal,BestSumHorizontal,IndexHorizontal),
	get_semi_vertical_line(Board,3,1,Player,PointsVertical,ListSumVertical,EmptySpacesVertical),
	select_best(PointsVertical,ListSumVertical,BestPosVertical,BestSumVertical,IndexVertical),

	append([BestPosPlus],[BestPosX],BestPos1),
	append(BestPos1,[BestPosDiagLineDec],BestPos2),
	append(BestPos2,[BestPosDiagLineCrs],BestPos3),
	append(BestPos3,[BestPosHorizontal],BestPos4),
	append(BestPos4,[BestPosVertical],BestPos5),
	
	append([BestSumPlus],[BestSumX],BestSum1),
	append(BestSum1,[BestSumDiagLineDec],BestSum2),
	append(BestSum2,[BestSumDiagLineCrs],BestSum3),
	append(BestSum3,[BestSumHorizontal],BestSum4),
	append(BestSum4,[BestSumVertical],BestSum5),
	
	!,member(4,BestSum5),
	
	nth1(IndexPlus,EmptySpacesPlus,FinalEmptySpacesPlus),
	nth1(IndexX,EmptySpacesX,FinalEmptySpacesX),
	nth1(IndexDiagLineDec,EmptySpacesDiagLineDec,FinalEmptySpacesDiagLineDec),
	nth1(IndexDiagLineCrs,EmptySpacesDiagLineCrs,FinalEmptySpacesDiagLineCrs),
	nth1(IndexHorizontal,EmptySpacesHorizontal,FinalEmptySpacesHorizontal),
	nth1(IndexVertical,EmptySpacesVertical,FinalEmptySpacesVertical),
	
	append([FinalEmptySpacesPlus],[FinalEmptySpacesX],EmptySpaces1),
	append(EmptySpaces1,[FinalEmptySpacesDiagLineDec],EmptySpaces2),
	append(EmptySpaces2,[FinalEmptySpacesDiagLineCrs],EmptySpaces3),
	append(EmptySpaces3,[FinalEmptySpacesHorizontal],EmptySpaces4),
	append(EmptySpaces4,[FinalEmptySpacesVertical],FinalEmptySpacesSet),
	
	select_best(BestPos5,BestSum5,_,_,IndexFinal),
	nth1(IndexFinal,FinalEmptySpacesSet,FinalEmptySpaces),
	nth1(1,FinalEmptySpaces,FinalPosition),
	nth1(1,FinalPosition,X),
	nth1(2,FinalPosition,Y),
	Block is X*10+Y.
	
computer_vs_computer_play(Board,Player,MaxPoints,MaxPieces,1,Dificulty2):-
	nl, write('Computador '), write(Player), write(' a jogar'),nl,nl,
	verify_semi_patterns(Board,Player,Position),
	argvl(Position,Line,Column),
	get_value(Board,Line,Column,Value),
	get_player_arg(Player,Value,A),
	get_new_board(Board,Position,A,NBoard,Player),
	printt(NBoard),
	verify_patterns(NBoard,Player,Points),
	print_points(Points),
	update_points(NBoard,Points,Player,UBoard),
	count_jokers(UBoard,1,Jokers),
	ask_jokers_computer(UBoard,Jokers,FinalBoard),
	printt(FinalBoard),
	count_points(FinalBoard,1,Points1,Points2),
	send(@scrore1,string(Points1)),
	send(@scrore2,string(Points2)),
	write_points(Points1,Points2,'Computador 1','Computador 2'),
	verify_endgame(FinalBoard,Player,'Computador',Points1,Points2,MaxPoints,MaxPieces),
	get_player(Player, NPlayer),
	sleep(6),
	computer_vs_computer_play(FinalBoard,NPlayer,MaxPoints,MaxPieces,Dificulty2,1).
	
computer_vs_computer_play(Board,Player,MaxPoints,MaxPieces,2,Dificulty2):-
	get_player(Player,PlayerForBlock),
	(
		verify_semi_patterns_block(Board,PlayerForBlock,Position);
		verify_semi_patterns(Board,Player,Position)
	),
	nl, write('Computador '), write(Player), write(' a jogar'),nl,nl,
	argvl(Position,Line,Column),
	get_value(Board,Line,Column,Value),
	get_player_arg(Player,Value,A),
	get_new_board(Board,Position,A,NBoard,Player),
	printt(NBoard),
	verify_patterns(NBoard,Player,Points),
	print_points(Points),
	update_points(NBoard,Points,Player,UBoard),
	count_jokers(UBoard,1,Jokers),
	ask_jokers_computer(UBoard,Jokers,FinalBoard),
	printt(FinalBoard),
	count_points(FinalBoard,1,Points1,Points2),
	send(@scrore1,string(Points1)),
	send(@scrore2,string(Points2)),
	write_points(Points1,Points2,'Computador 1','Computador 2'),
	verify_endgame(FinalBoard,Player,'Computador',Points1,Points2,MaxPoints,MaxPieces),
	get_player(Player, NPlayer),
	sleep(5),
	computer_vs_computer_play(FinalBoard,NPlayer,MaxPoints,MaxPieces,Dificulty2,2).
