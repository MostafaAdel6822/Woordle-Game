is_category(C):-
	word(_,C).
	
categories(List):-
	setof(C, is_category(C), List).
	
available_length(L):-
	word(X,_),
	atom_length(X,Y),
	Y == L.

pick_word(W,L,C):-
	word(W,C),
	atom_length(W,L).
	
correct_letters(L1,L2,XL):-
	sort(L1,L3),
	sort(L2,L4),
	intersection(L3,L4,XL).

correct_positions([],_,[]):-!.

correct_positions([H1|T1],[H2|T2],[H3|T3]):-
	H1 = H2,
	H3 = H1,
	correct_positions(T1,T2,T3).
	
correct_positions([H1|T1],[H2|T2],PL):-
	H1 \= H2,
	correct_positions(T1,T2,PL).
	
build_kb:-
	write('Please enter a word and its category on separate lines:'),nl,
	read(N),
	(
	 N = done;
	 read(C),
	 assert(word(N,C)),
	 build_kb
	).
		
checkC(X):- 
	is_category(X).
checkC(X):- 
	write('This category does not exist.'),nl,
	write('Choose a category:'),nl,
	read(Y),
	checkC(Y).

checkL(X,Y1):- 
	available_length(X),
	N is X+1,
	Y1=X,
	write('Game started. You have '),
	write(N),
	write(' guesses.'),nl,nl.
checkL(X,Y1):- 
	write('There are no words of this length.'),nl,
	write('Choose a length:'),nl,
	read(Y),
	checkL(Y,Y1).

checkZ(Z,Z1,Y,N):-	
	N == 1,
	Z \= Z1,
	write('You Lost!').
	
checkZ(Z,Z1,Y,N):-	
    N > 0,
	Z = Z1,
	write('You Won!').
	
checkZ(Z,Z1,Y,N):-	
	N > 0,
	atom_length(Z,L),
	L = Y,
	atom_chars(Z, L1),
	atom_chars(Z1, L2),
	correct_letters(L1,L2,ListO1),
	correct_positions(L1,L2,ListO2),	
	write('Correct letters are:'),
	write(ListO1),nl,
	write('Correct letters in correct positions are:'),
	write(ListO2),nl,
    N1 is N-1,
	write('Remaining Guesses are '),
	write(N1),nl,nl,
	write('Enter a word composed of '),
	write(Y),
	write(' letters:'),nl,
	read(Z3),
	checkZ(Z3,Z1,Y,N1).
	
checkZ(Z,Z1,Y1,N):-
        atom_length(Z,L),
	L \= Y1,
	write('Word is not composed of '), 
	write(Y1),
	write(' letters. Try again. '),nl,
	write('Remaining Guesses are '),
	write(N),nl,nl,
	write('Enter a word composed of '),
	write(Y1),
	write(' letters:'),nl,
	read(Z3),
	checkZ(Z3,Z1,Y1,N).
	
play:-
	write('The available categories are:'),
	categories(List),nl,
	write(List),nl,nl,
	write('Choose a category:'),nl,
	read(C),
	checkC(C),
	write('Choose a length:'),nl,
	read(Y),
	checkL(Y,Y1), 
	write('Enter a word composed of '),
	write(Y1),
	write(' letters:'),nl,
	read(Z),
	N is Y1+1,
	pick_word(Z1,Y1,C),
	atom_length(Z1,Y2),
	checkZ(Z,Z1,Y2,N).
	
main:-
	write('Welcome to Pro-Wordle!'),nl,
	write('----------------------'),nl,nl,
	build_kb,nl,
	play.

	

	
	
	
	
	