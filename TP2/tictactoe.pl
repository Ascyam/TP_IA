	/*********************************
	DESCRIPTION DU JEU DU TIC-TAC-TOE
	*********************************/

	/*
	Une situation est decrite par une matrice 3x3.
	Chaque case est soit un emplacement libre (Variable LIBRE), soit contient le symbole d'un des 2 joueurs (o ou x)

	Contrairement a la convention du tp precedent, pour modeliser une case libre
	dans une matrice on n'utilise pas une constante speciale (ex : nil, 'vide', 'libre','inoccupee' ...);
	On utilise plut�t un identificateur de variable, qui n'est pas unifiee (ex : X, A, ... ou _) .
	La situation initiale est une "matrice" 3x3 (liste de 3 listes de 3 termes chacune)
	o� chaque terme est une variable libre.	
	Chaque coup d'un des 2 joureurs consiste a donner une valeur (symbole x ou o) a une case libre de la grille
	et non a deplacer des symboles deja presents sur la grille.		
	
	Pour placer un symbole dans une grille S1, il suffit d'unifier une des variables encore libres de la matrice S1,
	soit en ecrivant directement Case=o ou Case=x, ou bien en accedant a cette case avec les predicats member, nth1, ...
	La grille S1 a change d'etat, mais on n'a pas besoin de 2 arguments representant la grille avant et apres le coup,
	un seul suffit.
	Ainsi si on joue un coup en S, S perd une variable libre, mais peut continuer a s'appeler S (on n'a pas besoin de la designer
	par un nouvel identificateur).
	*/
:- use_module(library(clpfd)).

situation_initiale([ [_,_,_],
					 [_,_,_],
					 [_,_,_] ]).


	% Convention (arbitraire) : c'est x qui commence

joueur_initial(x).


	% Definition de la relation adversaire/2

adversaire(x,o).
adversaire(o,x).


	/****************************************************
	 DEFINIR ICI a l'aide du predicat ground/1 comment
	 reconnaitre une situation terminale dans laquelle il
	 n'y a aucun emplacement libre : aucun joueur ne peut
	 continuer a jouer (quel qu'il soit).
	 ****************************************************/

situation_terminale(_J, S) :-  
	ground(S).

situation_terminale(J,S) :-
	alignement(A,S),
   	alignement_perdant(A,J), !.	

situation_terminale(J,S) :-
	alignement(A,S),
   	alignement_gagnant(A,J), !.

	/***************************
	DEFINITIONS D'UN ALIGNEMENT
	***************************/

alignement(L, Matrix) :- ligne(    L,Matrix).
alignement(C, Matrix) :- colonne(  C,Matrix).
alignement(D, Matrix) :- diagonale(D,Matrix).

	/********************************************
	 DEFINIR ICI chaque type d'alignement maximal 
 	 existant dans une matrice carree NxN.
	 ********************************************/
	
ligne(L, M) :- nth1(_,M,L).
 
colonne(C,M) :- 
	transpose(M,M2), 
	nth1(_,M2,C).

	/* Definition de la relation liant une diagonale D a la matrice M dans laquelle elle se trouve.
		il y en a 2 sortes de diagonales dans une matrice carree(https://fr.wikipedia.org/wiki/Diagonale) :
		- la premiere diagonale (principale)  : (A I)
		- la seconde diagonale                : (Z R)
		A . . . . . . . Z
		. \ . . . . . / .
		. . \ . . . / . .
		. . . \ . / . . .
		. . . . X . . .
		. . . / . \ . . . 
		. . / . . . \ . .
		. / . . . . . \ .
		R . . . . . . . I
	*/
		
diagonale(D, M) :- 
	premiere_diag(1,D,M).

diagonale(D, M) :-
	seconde_diag(1,D,M).

	
premiere_diag(_,[],[]).
premiere_diag(K,[E|D],[Ligne|M]) :-
	nth1(K,Ligne,E),
	K1 is K+1,
	premiere_diag(K1,D,M).

seconde_diag(_,[],[]).
seconde_diag(K,[E|D],[Ligne|M]) :-
	reverse(Ligne,LigR),
	nth1(K,LigR,E),
	K1 is K+1,
	seconde_diag(K1,D,M).


	/*****************************
	 DEFINITION D'UN ALIGNEMENT 
	 POSSIBLE POUR UN JOUEUR DONNE
	 *****************************/

possible([X|L], J) :- unifiable(X,J), possible(L,J).
possible(  [],  _).

	/* Attention 
	il faut juste verifier le caractere unifiable
	de chaque emplacement de la liste, mais il ne
	faut pas realiser l'unification.
	*/

% A FAIRE 
unifiable(X,J) :- 
	X=J;
	var(X).
	
	
	/**********************************
	 DEFINITION D'UN ALIGNEMENT GAGNANT
	 OU PERDANT POUR UN JOUEUR DONNE J
	 **********************************/
	/*
	Un alignement gagnant pour J est un alignement
possible pour J qui n'a aucun element encore libre.
	*/
	
	/*
	Remarque : le predicat ground(X) permet de verifier qu'un terme
	prolog quelconque ne contient aucune partie variable (libre).
	exemples :
		?- ground(Var).
		no
		?- ground([1,2]).
		yes
		?- ground(toto(nil)).
		yes
		?- ground( [1, toto(nil), foo(a,B,c)] ).
		no
	*/
		
	/* Un alignement perdant pour J est un alignement gagnant pour son adversaire. */

% A FAIRE

alignement_gagnant(Ali, J) :- 
	ground(Ali),
	possible(Ali,J).

alignement_perdant(Ali, J) :-
	adversaire(J,A), 
	alignement_gagnant(Ali, A).


	/* ****************************
	DEFINITION D'UN ETAT SUCCESSEUR
	****************************** */

	/* 
	Il faut definir quelle operation subit la matrice
	M representant l'Etat courant
	lorsqu'un joueur J joue en coordonnees [L,C]
	*/	

% A FAIRE
successeur(J, Etat,[L,C]) :-
	nth1(L,Etat,Lig),
	nth1(C,Lig,E),
	var(E),
	E=J.

	/**************************************
   	 EVALUATION HEURISTIQUE D'UNE SITUATION
  	 **************************************/

	/*
	1/ l'heuristique est +infini si la situation J est gagnante pour J
	2/ l'heuristique est -infini si la situation J est perdante pour J
	3/ sinon, on fait la difference entre :
	   le nombre d'alignements possibles pour J
	moins
 	   le nombre d'alignements possibles pour l'adversaire de J
*/


heuristique(J,Situation,H) :-		% cas 1
   H = 10000,				% grand nombre approximant +infini
   alignement(Alig,Situation),
   alignement_gagnant(Alig,J), !.
	
heuristique(J,Situation,H) :-		% cas 2
   H = -10000,				% grand nombre approximant -infini
   alignement(Alig,Situation),
   alignement_perdant(Alig,J), !.	


% on ne vient ici que si les cut precedents n'ont pas fonctionne,
% c-a-d si Situation n'est ni perdante ni gagnante.

% A FAIRE 					cas 3
heuristique(J,Situation,H) :-
	findall(1, (alignement(Ali, Situation),possible(Ali,J)), H1),
	adversaire(J, A),
	findall(1, (alignement(Ali, Situation),possible(Ali,A)), H2),
	length(H1, L1),
	length(H2, L2),
	H is (L1-L2).

test_alignement:-
	alignement_gagnant([x,x,x], x),
	alignement_perdant([x,x,x], o),
	not(alignement_gagnant([_,x,x], x)),
	not(alignement_gagnant([o,x,_], x)),
	not(alignement_perdant([x,o,x], x)).


test_heuristique :-
	[tictactoe], 
	situation_initiale(S),

	heuristique(x, S, 0),
	heuristique(o,[[o,_,_]
				  ,[_,x,x]
				  ,[o,_,_]], 0),	
	heuristique(o,[[o,_,x]
				  ,[_,x,_]
				  ,[o,_,_]], -6),
	heuristique(x,[[_,_,_]
				  ,[o,x,x]
				  ,[o,_,_]], 6),
	heuristique(x,[[o,_,_]
				  ,[o,x,x]
				  ,[o,_,_]], -10000),
	heuristique(x,[[o,_,_]
				  ,[x,x,x]
				  ,[o,_,_]], 10000),
	heuristique(x,[[x,_,_],
				   [_,o,x],
				   [_,x,o]],4),
	heuristique(o,[[x,_,_],
				   [_,o,x],
				   [_,x,o]],-4).
