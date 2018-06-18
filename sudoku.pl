


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

%% Nieme élément de la liste L avec la variable X. %%
element(1,[X|_],X).
element(Res,[_|Q],X):-element(N,Q,X), Res is N+1.

%%  retirer_element_n(N,L,NL)  unifie NL avec la liste obtenue en enlevant Ne ??ent de la liste L.%%
retirer_element_n(1,[T|Q],Q):-!.
retirer_element_n(N,[T|Q],[T|Reste]):-P is N-1, retirer_element_n(P,Q,Reste).

%%  retirer_element_x(X,L,NL) qui unifie avec la variable NL la liste obtenue en retirant l???ent unifi?avec X de la liste L. Si l???ent X n?appartient pas ?L,alors le pr?icat n??houera pas et NL=L.%%

retirer_element_x(_,[],[]).
retirer_element_x(X,[X|Q],Q):-!.
retirer_element_x(X,[T|Q],[T|Reste]):-retirer_element_x(X,Q,Reste).

%% retirer_liste(E,L,NL) qui unifie NL avec la liste L auquel on a retir?tous les ??ents de la liste E (s?ils existent)%%

retirer_liste([],L,L):-!.
retirer_liste([T|QE],L,NL):-retirer_element_x(T,L,Res),retirer_liste(QE,Res,NL).

%% set_element_n(N,L,X,NL) qui remplace le Ne ??ent de la liste L par l???ent unifi?avec X et qui unifie le r?ultat avec NL

set_element_n(1,[_|Q],X,[X|Q]):-!.
set_element_n(N,[T|Q],X,[T|Reste]):- P is N-1,set_element_n(P,Q,X,Reste).



varcheck([]).
varcheck([L|T]):- nonvar(L),varcheck(T).


imprime([]).
imprime([T|Q]) :- imprimeLigne(T),nl,imprime(Q).

imprimeLigne([]).
imprimeLigne([T|Q]) :- nonvar(T),write(T),tab(1),imprimeLigne(Q);
						write('_'),tab(1),imprimeLigne(Q).

imprimeIndice([]).
imprimeIndice([T|Q]) :- imprimeLigneIndice(T),nl,imprimeIndice(Q).

imprimeLigneIndice([]).
imprimeLigneIndice([T|Q]) :- write(T),tab(1),imprimeLigneIndice(Q).

aplatir([],[]).
aplatir([T|Q],R):-aplatir(T,R1),aplatir(Q,R2),concat(R1,R2,R),!.
aplatir(E,[E]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*regionSudoku([],[]).
regionSudoku([L1,L2,L3|Q],[R1,R2,R3|R]):- region(L1,L2,L3,R1,[RL1,RL2,RL3]),region(RL1,L2,L3,R1,Reste2),regionSudoku(Q,R).

%%(L1,L2,L3,R,Reste) Renvoie la premi?e region faisable de l1->l3 dans R, er retourne le reste
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
/* (M1,M2) Renvoie transpos?de M1 dans M2 */
trans([], []).
trans([[H|T] |Tail], [[H|NT] |NTail]) :-
	firstCol(Tail, NT, Rest), trans(Rest, NRest), firstCol(NTail, T, NRest).

/* (L1,L2,L3) met premi?e colonne dans L2 et Reste des lignes dans L3*/
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


solved(S):-get_coord_x(X,Y,S),!,fail.
solved(S):-valide(S).

genere_sudoku(S,NS):- get_coord_x(X,Y,S),!,get_possibles(X,Y,S,P),element(N,P,V),set_valeur(X,Y,S,V,NS).

solve(S,S):- solved(S).
solve(S,Solution):-genere_sudoku(S,NS),valide(NS),solve(NS,Solution).

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


solved([[1,4,3,9,8,6,2,5,7],
[6,7,9,4,2,5,3,8,1],
[2,8,5,7,3,1,6,9,4],
[9,6,2,3,5,4,1,7,8],
[3,5,7,6,1,8,9,4,2],
[4,1,8,2,7,9,5,6,3],
[8,2,1,5,6,7,4,3,9],
[7,9,6,1,4,3,8,2,5],
[5,3,4,8,9,2,7,1,6]]).

%grille(0,L).
%grille(N,L):- N>0,concat(L,liste(X),L),grille(N-1,L).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% AJOUT LEO 14/06

% verifier que les coordonnées sont valides en interne 0-8
validCoord(X):- X>=0, X=<8, integer(X), !.
validCoord(_):- fail.

% verifier que les coordonnées entrées par l'utilisateur sont valides 1-9
validUserInputNumber(X):- X>0, X=<9, integer(X), !.
validUserInputNumber(_):- nl, write('Saisie incorrecte !'),nl,nl, fail.

% verifier si la case est une case jouée
isPlayableCell(X,Y,Sudoku):- getElement(X,Y,Sudoku,Element), Element = ' ',!.
%% isPlayableCell(X,Y,_):- isJeuJoueur([X,Y]),!.
isPlayableCell(_,_,_):- write('Cette case n\'est pas jouable!'),nl,fail.



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% ===PROGRAMME PRINCIPAL=== %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sudoku :- nl,
    write('====================================================='),nl,
    write('======= Bienvenue dans notre programme sudoku ======='),nl,
    write('====================================================='),nl,nl,
    repeat, menu, !.
menu :- write('\t\t=====  MENU  ====='),nl,nl,
    write(' Que voulez vous faire ?'),nl,nl,
    write('1. Resoudre un sudoku'),nl,
    write('2. Proposer un sudoku'),nl,
    write('4. Quitter'),nl,nl,
    write('Que voulez-vous faire ? : '),
    read(Choice), nl,
    handle(Choice),
    Choice=4, nl.

handle(1):- write('---- RESOLUTION D\'UN SUDOKU ----'),nl,
                        l(S),
                        %% imprime(S),nl,
                        %% solve(S,NS),
                        %% imprime(NS),nl,
                        asserta(sudokuGrid(S)),
                        repeat,
                        userSolvingSudoku,
                        !.
handle(2):- write('---- PROPOSER UN SUDOKU ----'), nl, !.
handle(4):- write('---- Au revoir ! ----'),!.
handle(_):- write('---- Option invalide ----'),!.

userSolvingSudoku :- nl,write('---- Complétez le sudoku ----'),nl,nl,
    sudokuGrid(S), imprime(S), nl,
    write('1. Definir numero'), nl,
    write('2. Effacer numero'), nl,
    write('4. Quitter'), nl, nl,
    write('Que voulez-vous faire ? : '),
    read(Choice), nl,
    handleResolution(Choice,S),
    sudokuGrid(ModifiedGrid),
    isProgramFinished(ModifiedGrid,Choice). %% ???

% Definir d'un numero
handleResolution(1,S):- write('Ligne de la case à modifier : '), read(X), validUserInputNumber(X),nl,
    write('Colonne de la case à modifier :'), read(Y), validUserInputNumber(Y),nl,
    write('Valeur de la case : '), read(N), validUserInputNumber(N),nl,
    X1 is (X-1), Y1 is (Y-1),
    isPlayableCell(X1,Y1,S), %% ???
    changer(N,X1,Y1,S,S1), %% ???
    verification(X1,Y1,S1), %% ???
    retract(sudokuGrid(S)),
    asserta(sudokuGrid(S1)),
    addPlayerMove([X1,Y1]), %% ???
    write('Succès de l\'ajout.'),nl,!.

handleResolution(1,_):- !.

% Effacer un numero
handleResolution(2,S):- write('Ligne de la case à effacer :'), read(X), validUserInputNumber(X),nl,
    write('Colonne de la case à effacer:'), read(Y), validUserInputNumber(Y),nl,
    X1 is (X-1), Y1 is (Y-1),
    isPlayableCell(X1,Y1,S), %% ???
    isCellCompleted(X1,Y1,S), %% ???
    changer(' ',X1,Y1,S,S1), %% ???
    retract(sudokuGrid(S)),
    asserta(sudokuGrid(S1)),
    deletePlayerMove([X,Y]),!. %% ???

handleResolution(2,_):- !.

handleResolution(4,_):-!.

handleResolution(_,_):- nl, write('Option invalide'), nl, !, fail.
