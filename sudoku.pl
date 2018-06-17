


% liste(X), imprime(X).
liste([
          [X11, X12, X13, X14, X15, X16, X17, X18, X19],
          [X21, X22, X23, X24, X25, X26, X27, X28, X29],
          [X31, X32, X33, X34, X35, X36, X37, X38, X39],
          [X41, X42, X43, X44, X45, X46, X47, X48, X49],
          [X51, X52, X53, X54, X55, X56, X57, X58, X59],
          [X61, X62, X63, X64, X65, X66, X67, X68, X69],
          [X71, X72, X73, X74, X75, X76, X77, X78, X79],
          [X81, X82, X83, X84, X85, X86, X87, X88, X89],
          [X91, X92, X93, X94, X95, X96, X97, X98, X99]
        ]).
		
sudoVide([
          [x,x,x,x,x,x,x,x,x],
          [x,x,x,x,x,x,x,x,x],
		  [x,x,x,x,x,x,x,x,x],
		  [x,x,x,x,x,x,x,x,x],
		  [x,x,x,x,x,x,x,x,x],
		  [x,x,x,x,x,x,x,x,x],
		  [x,x,x,x,x,x,x,x,x],
		  [x,x,x,x,x,x,x,x,x],
		  [x,x,x,x,x,x,x,x,x]
        ]).
/* generate-and-test paradigm */

%%%%%%%%%%% Base List predicates %%%%%%%%%%%%%%
longueur([],0).
longueur([T|Q],N):-longueur(Q,P),N is P+1.

concat([],L,L).
concat([T|Q],L,[T|R]) :- concat(Q,L,R).

partition(_,[],[],[]).
partition(X,[T|Q],[T|Linf],Lsup) :- nonvar(T),T<X, partition(X,Q,Linf,Lsup),!.
partition(X,[T|Q],Linf,[T|Lsup]) :- nonvar(T),T>X, partition(X,Q,Linf,Lsup),!.
partition(X,[T|Q],Linf,Lsup) :- nonvar(T),T==X, partition(X,Q,Linf,Lsup),!.
partition(X,[T|Q],[T|Linf],Lsup) :- partition(X,Q,Linf,Lsup).

tri([],[]).
tri([T|Q],Res) :- nonvar(T),partition(T,Q,Linf,Lsup),tri(Linf,LinfTrie),tri(Lsup,LsupTrie),concat(LinfTrie,[T|LsupTrie],Res),!.
tri([T|Q],Res) :- partition(0,Q,Linf,Lsup),tri(Linf,LinfTrie),tri(Lsup,LsupTrie),concat(LinfTrie,[T|LsupTrie],Res),!.

%% Nième élément de la liste L avec la variable X. %%
element(1,[X|_],X).
element(Res,[_|Q],X):-element(N,Q,X), Res is N+1.

%%  retirer_element_n(N,L,NL)  unifie NL avec la liste obtenue en enlevant Ne élément de la liste L.%%
retirer_element_n(1,[T|Q],Q):-!.
retirer_element_n(N,[T|Q],[T|Reste]):-P is N-1, retirer_element_n(P,Q,Reste).

%%  retirer_element_x(X,L,NL) qui unifie avec la variable NL la liste obtenue en retirant l’élément unifié avec X de la liste L. Si l’élément X n’appartient pas à L,alors le prédicat n’échouera pas et NL=L.%% 

retirer_element_x(_,[],[]).
retirer_element_x(X,[X|Q],Q):-!.
retirer_element_x(X,[T|Q],[T|Reste]):-retirer_element_x(X,Q,Reste).

%% retirer_liste(E,L,NL) qui unifie NL avec la liste L auquel on a retiré tous les éléments de la liste E (sils existent) %%

retirer_liste([],L,L):-!.
retirer_liste([T|QE],L,NL):-retirer_element_x(T,L,Res),retirer_liste(QE,Res,NL).

%% set_element_n(N,L,X,NL) qui remplace le Ne élément de la liste L par l’élément unifié avec X et qui unifie le résultat avec NL

set_element_n(1,[_|Q],X,[X|Q]):-!.
set_element_n(N,[T|Q],X,[T|Reste]):- P is N-1,set_element_n(P,Q,X,Reste).



varcheck([]).
varcheck([L|T]):- nonvar(L),varcheck(T).

aplatir([],[]).
aplatir([T|Q],R):-aplatir(T,R1),aplatir(Q,R2),concat(R1,R2,R),!.
aplatir(E,[E]).


%%%% Affichage %%%%

imprime(S) :- nl,imprime3Lignes(S),!,write('-------------------------').

imprime3Lignes([]).
imprime3Lignes([L1,L2,L3|Q]):- write('-------------------------'),nl,imprimeLigne(L1),nl,imprimeLigne(L2),nl,imprimeLigne(L3),nl,imprime3Lignes(Q).

imprimeLigne([]):- write('|').
imprimeLigne([V1,V2,V3|Q]) :- write('|'),tab(1),imprimeValeur(V1),tab(1),imprimeValeur(V2),tab(1),imprimeValeur(V3),tab(1),imprimeLigne(Q).
            
imprimeValeur(V):- V\=x, write(V);write('_').

%% AFFICHAGE JEU %%

imprimeCalque(S,B):- nl,imprime3LignesCalque(S,B),!,write('-----------------------------------').

imprime3LignesCalque([],_).
imprime3LignesCalque([L1,L2,L3|Q],[L1B,L2B,L3B|QB]):- write('-----------------------------------'),nl,imprimeLigneCalque(L1,L1B),nl,imprimeLigneCalque(L2,L2B),nl,imprimeLigneCalque(L3,L3B),nl,imprime3LignesCalque(Q,QB).

imprimeLigneCalque([],_):- write('|').
imprimeLigneCalque([V1,V2,V3|Q],[V1B,V2B,V3B|QB]) :- write('|'),tab(1),imprimeValeurCalque(V1,V1B),imprimeValeurCalque(V2,V2B),imprimeValeurCalque(V3,V3B),imprimeLigneCalque(Q,QB).
  
imprimeValeurCalque(V,VB):- V\=x, VB\=y,tab(1), write(V),tab(1),!.
imprimeValeurCalque(V,VB):- V\=x, VB==y,write(V),write('~'),tab(1),!.
imprimeValeurCalque(V,VB):- tab(1),write('_'),tab(1).

%% Affichage indice %%

imprimeIndice([]).
imprimeIndice([T|Q]) :-imprimeLigneIndice(T),nl,imprimeIndice(Q).

imprimeLigneIndice([]).
imprimeLigneIndice([T|Q]) :-write(T),tab(1),imprimeLigneIndice(Q).

%%%% -------- %%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*regionSudoku([],[]).
regionSudoku([L1,L2,L3|Q],[R1,R2,R3|R]):- region(L1,L2,L3,R1,[RL1,RL2,RL3]),region(RL1,L2,L3,R1,Reste2),regionSudoku(Q,R).

%%(L1,L2,L3,R,Reste) Renvoie la première region faisable de l1->l3 dans R, er retourne le reste
region([],[],[],[],[]).
region([X11,X12,X13|RL1],[X21,X22,X23|RL2],[X31,X32,X33|RL3],[X11,X12,X13,X21,X22,X23,X31,X32,X33],[RL1,RL2,RL3]).
*/


region3Lignes([],[],[],[]).
region3Lignes([X1,X2,X3|RL1], [X4,X5,X6|RL2], [X7,X8,X9|RL3], [[X1,X2,X3,X4,X5,X6,X7,X8,X9]|RR]):-region3Lignes(RL1,RL2,RL3,RR).

region_sudoku([],[]).
region_sudoku([L1,L2,L3|RL],[TR|RR]):-region3Lignes(L1,L2,L3,TR),region_sudoku(RL,RR).

region(S,[R1,R2,R3,R4,R5,R6,R7,R8,R9]):-region_sudoku(S,[[R1,R2,R3],[R4,R5,R6],[R7,R8,R9]]).

get_region(X,Y,S,R):-region(S,Regions),NReg is (((X-1) //3 *3 + 1) + ((Y-1) // 3)),element(NReg,Regions,R).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/* (M1,M2) Renvoie transposé de M1 dans M2 */
trans([], []).
trans([[H|T] |Tail], [[H|NT] |NTail]) :- 
	firstCol(Tail, NT, Rest), trans(Rest, NRest), firstCol(NTail, T, NRest).
	
/* (L1,L2,L3) met première colonne dans L2 et Reste des lignes dans L3*/
firstCol([], [], []).
firstCol([[H|T] |Tail], [H|Col], [T|Rows]) :- firstCol(Tail, Col, Rows).

/* pas bon, remplace var par des 1
domainCheck([]).
domainCheck([H|T]) :- between(1,9,H), domainCheck(T),!.
*/
%%%%%%%%%%%%%%
/*Liste simple*/
 
domainCheck([]).
domainCheck([T|Q]) :- member(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]), domainCheck(Q),!.


%%% A REFAIRE UNE FOIS PLUS AVANCE
differentes([]).
differentes([T|Q]) :- ligneDifferente(T),differentes(Q).

%% get_possibles(X,Y,S,P) %%


get_possibles(X,Y,S,P):-element(X,S,Ligne),element(Y,Ligne,ValCel),ValCel==x,trans(S,Cols),element(Y,Cols,Col),get_region(X,Y,S,Region),get_possibles_forLCR(Ligne,Col,Region,P),!.
get_possibles(X,Y,S,P):-element(X,S,Ligne),element(Y,Ligne,ValCel),
						ValCel\=x,retirer_element_x(ValCel,Ligne,LigneSansVal),
						trans(S,Cols),element(Y,Cols,Col),retirer_element_x(ValCel,Col,ColSansVal),
						get_region(X,Y,S,Region),retirer_element_x(ValCel,Region,RegionSansVal),
						get_possibles_forLCR(LigneSansVal,ColSansVal,RegionSansVal,P),!.
			
get_possibles_forLCR(Ligne,Col,Region,Possible):-L=[1,2,3,4,5,6,7,8,9],retirer_liste(Ligne,L,SansLigne),retirer_liste(Col,SansLigne,SansCol),retirer_liste(Region,SansCol,Possible).

get_valeur(X,Y,S,V):-element(X,S,Ligne),element(Y,Ligne,V).

set_valeur(1,Y,[T|Q],V,[NT|Q]):-set_element_n(Y,T,V,NT),!.
set_valeur(X,Y,[T|Q],V,[T|NQ]):-P is X-1,set_valeur(P,Y,Q,V,NQ).

case_valide(X,Y,S):-get_possibles(X,Y,S,P),P\=[],get_valeur(X,Y,S,V),V==x,!.
case_valide(X,Y,S):-get_possibles(X,Y,S,P),P\=[],get_valeur(X,Y,S,V),element(N,P,V),!.


case_non_valide(X,Y,S):- \+case_valide(X,Y,S).

valide(S):-get_valeur(X,Y,S,V),case_non_valide(X,Y,S),!,fail.
valide(S).

get_coord_x(X,Y,S):-get_valeur(X,Y,S,x).

get_coord_y(X,Y,S):-get_valeur(X,Y,S,y).

%%%%%%%%%%%%%%%% Generate and test %%%%%%%%%%%%%%%%%%%%%%


solved(S):-get_coord_x(X,Y,S),!,fail.
solved(S):-valide(S).

genere_sudoku(S,NS):- get_coord_x(X,Y,S),!,get_possibles(X,Y,S,P),element(N,P,V),set_valeur(X,Y,S,V,NS).

solve(S,S):- solved(S).
solve(S,Solution):-genere_sudoku(S,NS),valide(NS),solve(NS,Solution).

%%%%%%%%%%%%%%%% Random sudoku %%%%%%%%%%%%%%%%%%%%%%

generated(S):- get_coord_y(X,Y,S),!,fail.
generated(S):- valide(S).

genere_random_value(X,Y,S,V):- get_possibles(X,Y,S,P),longueur(P,Long),BornSup is Long+1,random(1,BornSup,N),element(N,P,V).

genere_random_sudoku(S,S):- generated(S),!.
genere_random_sudoku(S,Solution):- get_coord_y(X,Y,S),!,genere_random_value(X,Y,S,V),set_valeur(X,Y,S,V,NS),genere_random_sudoku(NS,Solution).

gen_r_repeat(S,NS):- repeat,genere_random_sudoku(S,NS),solve(NS,Solved),!.

%% genere_random_value(S,NS),valide(NS),genere_random_sudoku(NS,Solution).


genere_random_coord(X,Y):- P is 10,random(1,P,X),random(1,P,Y).

genere_random_grid_y(0,S,S):- genere_random_coord(X,Y),set_valeur(X,Y,S,y,NS),!.
genere_random_grid_y(N,S,RS):- genere_random_coord(X,Y),set_valeur(X,Y,S,y,NS),P is N-1,genere_random_grid_y(P,NS,RS).


%change_directory('C:/Users/MegaB/Documents/GitHub/IA02_sudokuProject').
%consult('sudoku.pl').
% vide(S),genere_random_grid_y(20,S,NS),gen_r_repeat(NS,R),imprime(NS),imprime(R).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
solve(S,S):- \+valide(S),!.
solve(S,S):-solved(S),!.
solve(S,Res):-get_coord_x(X,Y,S),!,get_possibles(X,Y,S,P),element(N,P,V),set_valeur(X,Y,S,V,NS),imprime(NS),nl,!,solve(NS,Res).
*/
%change_directory('C:/Users/mohamed/Desktop/IA02 Project').
%consult('projet.pl').
%l(S),solve_and_test(S,NS).

/*
solve_and_test(S,R):-solve(S,R),solved(R).
genere_possibles(X,Y,S,V):- get_possibles(X,Y,S,P),element(N,P,V).
*/

%genere_sudoku(S,S):- \+valide(S),!.
%genere_sudoku(S,[]):- get_coord_x(X,Y,S),genere_possibles(X,Y,S,V),set_valeur(X,Y,S,V,NS),valide(NS),imprime(NS),nl,genere_sudoku(NS,Res),!.

%genere_sudoku(S,NS):- get_coord_x(X,Y,S),genere_possibles(X,Y,S,V),set_valeur(X,Y,S,V,NS),imprime(NS).


%solve(S,Res):- genere_sudoku(S,Res),print("----------1----------"),solved(Res).


% exempleValide1(S),get_coord_x(X,Y,S).%

/*
ligneDifferente(Desordre) :- sort(Desordre,Trie),length(Trie, TailleTrie),length(Desordre,TailleDesordre),TailleDesordre==TailleTrie.
*/

%% [5,5,8,2,4,A,6] %%
%% [5,8,2,4,A,6] %%


ligneDifferente(Desordre) :- tri(Desordre,Trie),longueur(Trie, TailleTrie),longueur(Desordre,TailleDesordre),TailleDesordre==TailleTrie.

	
l([
        [x,4,3,x,8,x,2,5,x],
        [6,x,x,x,x,x,x,x,x],
        [x,x,x,x,x,1,x,9,4],
        [9,x,x,x,x,4,x,7,x],
        [x,x,x,6,x,8,x,x,x],
        [x,1,x,2,x,x,x,x,3],
        [8,2,x,5,x,x,x,x,x],
        [x,x,x,x,x,x,x,x,5],
        [x,3,4,x,9,x,7,1,x]
  ]).
  
lfacile([
        [x,9,x,4,x,x,3,x,x],
        [x,x,x,x,7,x,x,x,x],
        [1,x,6,3,x,9,x,4,2],
        [3,4,x,8,x,x,2,x,7],
        [x,5,7,2,x,1,9,3,x],
        [9,x,8,x,x,5,x,1,6],
        [8,7,x,5,x,4,1,x,9],
        [x,x,x,x,8,x,x,x,x],
        [x,x,4,x,x,7,x,8,x]
  ]).
  
exempleValide1([
			[9,2,8,4,1,7,6,5,3],
			[6,3,1,5,9,2,7,4,8],
			[7,5,4,6,8,3,1,2,9],
			[4,9,5,3,7,6,2,8,1],
			[1,7,3,9,2,8,5,6,4],
			[2,8,6,1,4,5,3,9,7],
			[8,1,2,7,5,4,9,3,6],
			[3,4,7,2,6,9,8,1,5],
			[5,6,9,8,3,1,4,7,2]
]).

l2([ 
[x,x,1,x,3,2,x,4,9],
[x,3,x,6,x,x,x,5,x],
[6,x,x,1,x,x,7,8,x],
[2,6,x,x,x,x,1,x,5],
[x,x,3,x,x,x,4,x,x],
[7,x,5,x,x,x,x,3,2],
[x,8,6,x,x,9,x,x,4],
[x,2,x,x,x,7,x,1,x],
[1,9,x,4,5,x,3,x,x] 
]).

test_poss([
[1,4,3,7,8,9,2,5,6],
[6,5,2,4,3,x,1,8,x],
[x,x,x,x,x,1,x,9,4],
[9,x,x,x,x,4,x,7,x],
[x,x,x,6,x,8,x,x,x],
[x,1,x,2,x,x,x,x,3],
[8,2,x,5,x,x,x,x,x],
[x,x,x,x,x,x,x,x,5],
[x,3,4,x,9,x,7,1,x]
]).
 
vide([
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x],
[x,x,x,x,x,x,x,x,x]
]).

rGridEx([
[x,x,x,y,y,y,x,x,y],
[x,x,x,y,y,y,x,x,y],
[x,x,x,y,y,y,x,x,y],
[x,x,x,x,x,x,x,x,y],
[x,x,x,x,x,x,x,x,y],
[x,x,x,x,x,x,x,x,y],
[x,x,x,x,x,x,y,x,y],
[x,x,x,x,x,x,x,x,y],
[x,x,x,x,y,x,x,x,y]
]).

solved([[1,4,3,9,8,6,2,5,7],
[6,7,9,4,2,5,3,8,1],
[2,8,5,7,3,1,6,9,4],
[9,6,2,3,5,4,1,7,8],
[3,5,7,6,1,8,9,4,2],
[4,1,8,2,7,9,5,6,3],
[8,2,1,5,6,7,4,3,9],
[7,9,6,1,4,3,8,2,5],
[5,3,4,8,9,2,7,1,6]]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%% USER INTERFACE %%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


start :- nl,nl,
		write('***************************************************'),nl,
		write('*** Vous avez lancé le programme ProjectSudoku ***'),nl,
		write('***************************************************'),
		queFaire,!.

queFaire :- nl,write('Que voulez vous faire ?'),nl,
			tab(2),write('1. Resoudre un sudoku'),nl,
			tab(2),write('2. Generer un sudoku'),nl,
			tab(2),write('3. Jouer a un sudoku'),nl,
			tab(2),write('4. Fin de programme'),nl,
			write(' ---->'),tab(1),read(Option),
			interpreterMenu(Option).

%% On rajoute sudoAResoudre dans la bdd des faits, il sera modifier jusqu'a resolution
interpreterMenu(1):- sudoVide(S),retractall(sudoAResoudre(_)),asserta(sudoAResoudre(S)),repeat,resoudre_sudo_user,!.
interpreterMenu(2):- sudoVide(S),retractall(randomSudo(_)),asserta(randomSudo(S)),repeat,generer_sudo_user,!.
interpreterMenu(3):- sudoVide(S),genere_random_grid_y(7,S,NS),gen_r_repeat(NS,R),
					 retractall(jouerSudo(_)),asserta(jouerSudo(R)),
					 retractall(baseJouerSudo(_)),asserta(baseJouerSudo(NS)),
					 repeat,jouer_sudo_user,!.
interpreterMenu(4).

% sudoVide(S),genere_random_grid_y(S,NS),,retractall(sudoRandom(_))
%change_directory('C:/Users/MegaB/Documents/GitHub/IA02_sudokuProject').
%consult('sudoku.pl').

%%%%%%%%%%%%    Jouer   %%%%%%%%%%%%

jouer_sudo_user:- 
			nl,write('Voici la grille actuelle :'),nl,
			jouerSudo(S),baseJouerSudo(B),imprimeCalque(S,B),nl,
			write('Que voulez vous faire ?'),nl,
			tab(2),write('1. Ajouter/Modifier une valeur au sudoku'),nl,
			tab(2),write('2. Retirer une valeur au sudoku'),nl,
			tab(2),write('3. Faire valider sudoku'),nl,
			tab(2),write('4. Resoudre Sudoku'),nl,
			tab(2),write('5. Fin de programme'),nl,
			write(' ---->'),tab(1),read(Option),
			interpreterJeu(Option).

check_not_base(X,Y,B) :- \+get_coord_y(X,Y,B).
			
interpreterJeu(1):- read_coord_user(X,Y),
					baseJouerSudo(B), check_not_base(X,Y,B),
					jouerSudo(S),read_valeur(V),
					set_valeur(X,Y,S,V,NS),
					valide(NS),
					retract(jouerSudo(S)),
					asserta(jouerSudo(NS)),
				    write('** Ajout réussi **'),nl,jouer_sudo_user,!.
					
interpreterJeu(1):- write('**** AJOUT INVALIDE ****'),jouer_sudo_user,!.

interpreterJeu(2):- read_coord_user(X,Y),
					baseJouerSudo(B), check_not_base(X,Y,B),
					jouerSudo(S),
					set_valeur(X,Y,S,x,NS),
					valide(NS),
					retract(jouerSudo(S)),
					asserta(jouerSudo(NS)),
					write('** Suppression réussie **'),jouer_sudo_user,!.

interpreterJeu(2):- write('**** SUPPRESSION INVALIDE ****'),jouer_sudo_user,!.

interpreterJeu(3):- jouerSudo(S),solved(S),write('** FELICITAION !**'),!.

interpreterJeu(3):- write('** Le sudoku n\'est pas valide **'),jouer_sudo_user,!.

interpreterJeu(5).
				
interpreterJeu(X):- X>5,write('** Erreur choix **'),jouer_sudo_user.

recup_base_sudo(S,B,SB):- get_coord_y(X,Y,B),!,get_valeur(X,Y,S,V),set_valeur(X,Y,B,V,SB),set_valeur(X,Y,B,x,NB),recup_base_sudo(S,NB,SB).
%%%%%%%%%%%% resolution %%%%%%%%%%%%

resoudre_sudo_user:- 
			nl,write('Voici la grille actuelle :'),nl,
			sudoAResoudre(S),imprime(S),nl,
			write('Que voulez vous faire ?'),nl,
			tab(2),write('1. Ajouter/Modifier une valeur au sudoku'),nl,
			tab(2),write('2. Retirer une valeur au sudoku'),nl,
			tab(2),write('3. Resoudre sudoku'),nl,
			tab(2),write('4. Fin de programme'),nl,
			write(' ---->'),tab(1),read(Option),
			interpreterResolution(Option).

interpreterResolution(1):- read_coord_user(X,Y),
						   read_valeur(V),
						   sudoAResoudre(S),
						   set_valeur(X,Y,S,V,NS),
						   valide(NS),
						   retract(sudoAResoudre(S)),
						   asserta(sudoAResoudre(NS)),
						   write('** Ajout réussi **'),nl,resoudre_sudo_user,!.
						   
interpreterResolution(1):-  write('**** erreur : Valeur dans ligne/region/colone  ****'),resoudre_sudo_user.
						   
interpreterResolution(2):- read_coord_user(X,Y),
						   sudoAResoudre(S),
						   set_valeur(X,Y,S,x,NS),
						   valide(NS),
						   retract(sudoAResoudre(S)),
						   asserta(sudoAResoudre(NS)),
						   write('** Suppression réussie **'),resoudre_sudo_user.
						   
interpreterResolution(3):- sudoAResoudre(S),solve(S,NS),
						   write('Voici la solution'),nl,
						   imprime(NS),!.

interpreterResolution(3):- nl,write('** Pas de solutions **').

interpreterResolution(4).

interpreterResolution(X):- X>4,write('** Erreur choix **'),resoudre_sudo_user.

%%%%%%%%%%%% resolution %%%%%%%%%%%%

generer_sudo_user:- 
			nl,write('Voici la grille actuelle :'),nl,
			randomSudo(S),imprime(S),nl,
			write('Que voulez vous faire ?'),nl,
			tab(2),write('1. Générer des valeur aléatoires'),nl,
			tab(2),write('2. Resoudre la grille'),nl,
			tab(2),write('4. Fin de programme'),nl,
			write(' ---->'),tab(1),read(Option),
			interpreterGeneration(Option).
			
interpreterGeneration(1):- read_nb_cases(NB),
						   randomSudo(S),
						   genere_random_grid_y(NB,S,NS),
						   gen_r_repeat(NS,R),
						   retract(randomSudo(S)),
						   asserta(randomSudo(R)),
						   nl,write('Generation réussie'),generer_sudo_user.
						   
interpreterGeneration(2):- randomSudo(S),
						   solve(S,NS),
						   nl,write('** Voici la solution **'),nl,
						   imprime(NS).
						   

interpreterGeneration(4).




read_coord_user(X,Y):- nl,write('Ligne de la case : '),nl,write(' ---->'), read(X),read_valide(X),nl,
					   write('Colonne de la case : '),nl,write(' ---->'), read(Y),read_valide(Y),nl,!.

read_coord_user(_,_):- nl,write('***** COORDONNEES INVALIDES *****'),nl,fail.

read_valeur(V):- write('valeur de la case : '),nl,write(' ---->'), read(V),read_valide(V),nl,!.

read_valeur(V):- nl,write('***** VALEUR INVALIDE *****'),nl,fail.

read_valide(X):- X>0,X<10.

read_nb_cases(NB) :- write('nombre de cases au plus à changer : '),nl,write(' ---->'), read(NB),nl,!.

%grille(0,L).
%grille(N,L):- N>0,concat(L,liste(X),L),grille(N-1,L).