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
		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%% ** Predicats de manipulation de listes ** %%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

aplatir([],[]).
aplatir([T|Q],R):-aplatir(T,R1),aplatir(Q,R2),concat(R1,R2,R),!.
aplatir(E,[E]).

%%%% ------------------------------------------------------------ %%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% ** Affichage ** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%% ------------------------------------------------------------ %%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%% ** Récuperation Colonnes/ Regions ** %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

%%%% ------------------------------------------------------------ %%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% ** Resolution ** %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

domainCheck([]).
domainCheck([T|Q]) :- member(A, [1, 2, 3, 4, 5, 6, 7, 8, 9]), domainCheck(Q),!.

differentes([]).
differentes([T|Q]) :- ligneDifferente(T),differentes(Q).


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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% ** Generate-and-test ** %%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

solved(S):-get_coord_x(X,Y,S),!,fail.
solved(S):-valide(S).

genere_sudoku(S,NS):- get_coord_x(X,Y,S),!,get_possibles(X,Y,S,P),element(N,P,V),set_valeur(X,Y,S,V,NS).

solve(S,S):- solved(S).
solve(S,Solution):-genere_sudoku(S,NS),valide(NS),solve(NS,Solution).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%% ** Generer random Sudoku ** %%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

generated(S):- get_coord_y(X,Y,S),!,fail.
generated(S):- valide(S).

genere_random_value(X,Y,S,V):- get_possibles(X,Y,S,P),longueur(P,Long),BornSup is Long+1,random(1,BornSup,N),element(N,P,V).

genere_random_sudoku(S,S):- generated(S),!.
genere_random_sudoku(S,Solution):- get_coord_y(X,Y,S),!,genere_random_value(X,Y,S,V),set_valeur(X,Y,S,V,NS),genere_random_sudoku(NS,Solution).

gen_r_repeat(S,NS):- repeat,genere_random_sudoku(S,NS),solve(NS,Solved),!.

genere_random_coord(X,Y):- P is 10,random(1,P,X),random(1,P,Y).

genere_random_grid_y(0,S,S):- genere_random_coord(X,Y),set_valeur(X,Y,S,y,NS),!.
genere_random_grid_y(N,S,RS):- genere_random_coord(X,Y),set_valeur(X,Y,S,y,NS),P is N-1,genere_random_grid_y(P,NS,RS).

ligneDifferente(Desordre) :- tri(Desordre,Trie),longueur(Trie, TailleTrie),longueur(Desordre,TailleDesordre),TailleDesordre==TailleTrie.

%%%% ------------------------------------------------------------ %%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%% ** USER INTERFACE ** %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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

interpreterMenu(1):- sudoVide(S),retractall(sudoAResoudre(_)),asserta(sudoAResoudre(S)),repeat,resoudre_sudo_user,!.
interpreterMenu(2):- sudoVide(S),retractall(randomSudo(_)),asserta(randomSudo(S)),repeat,generer_sudo_user,!.
interpreterMenu(3):- sudoVide(S),genere_random_grid_y(7,S,NS),gen_r_repeat(NS,R),
					 retractall(jouerSudo(_)),asserta(jouerSudo(R)),
					 retractall(baseJouerSudo(_)),asserta(baseJouerSudo(NS)),
					 repeat,jouer_sudo_user,!.
interpreterMenu(4).

%%%%%%%%%%%%%%%%%%%%%%%%% ** Resolution ** %%%%%%%%%%%%%%%%%%%%%%%%%%%

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
						   write('** Suppression réussie **'),resoudre_sudo_user,!.
						   
interpreterResolution(3):- sudoAResoudre(S),solve(S,NS),
						   write('Voici la solution'),nl,
						   imprime(NS),!.

interpreterResolution(3):- nl,write('** Pas de solutions **').

interpreterResolution(4):-!.

interpreterResolution(_):- messageErrorOption,fail.


%%%%%%%%%%%%%%%%%%%%%%%%% ** Generer ** %%%%%%%%%%%%%%%%%%%%%%%%%%%

generer_sudo_user:- 
			nl,write('Voici la grille actuelle :'),nl,
			randomSudo(S),imprime(S),nl,
			write('Que voulez vous faire ?'),nl,
			tab(2),write('1. Générer des valeur aléatoires'),nl,
			tab(2),write('2. Jouer à la grille'),nl,
			tab(2),write('3. Resoudre la grille'),nl,		
			tab(2),write('4. Fin de programme'),nl,
			write(' ---->'),tab(1),read(Option),
			interpreterGeneration(Option).
			
interpreterGeneration(1):- read_nb_cases(NB),
						   randomSudo(S),
						   genere_random_grid_y(NB,S,NS),
						   gen_r_repeat(NS,R),
						   retract(randomSudo(S)),
						   asserta(randomSudo(R)),
						   nl,write('Generation réussie'),generer_sudo_user,!.
			
interpreterGeneration(2):- randomSudo(S),getCalque(S,NS),
					 retractall(jouerSudo(_)),asserta(jouerSudo(S)),
					 retractall(baseJouerSudo(_)),asserta(baseJouerSudo(NS)),
					 repeat,jouer_sudo_user,!.
	
interpreterGeneration(3):- randomSudo(S),
						   solve(S,NS),
						   nl,write('** Voici la solution **'),nl,
						   imprime(NS),!.
interpreterGeneration(4):-!.

interpreterGeneration(_):- messageErrorOption,fail.

%%%%%%%%%%%%%%%%%%%%%%%%%%%% ** Jouer ** %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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

interpreterJeu(4):- jouerSudo(S),baseJouerSudo(B),recup_sudo_calque(S,B,SB),solve(SB,Solved),nl,write('** --Solution-- **'),imprimeCalque(Solved,B),!.


interpreterJeu(5).
				
interpreterJeu(_):- messageErrorOption,fail.

%%%%%%%%%%%%%%%%%%%%% ** Predicats dans gestion UI ** %%%%%%%%%%%%%%%%%%%%%%%

recup_sudo_calque(_,B,Res,Res):- valide(B),!.
recup_sudo_calque(S,B,ORes,Res):- get_coord_y(X,Y,B),!,get_valeur(X,Y,S,V),set_valeur(X,Y,ORes,V,Tmp),set_valeur(X,Y,B,x,NB),recup_sudo_calque(S,NB,Tmp,Res).

recup_sudo_calque(S,B,SB):-vide(X),recup_sudo_calque(S,B,X,SB).

getCalque([],[]).
getCalque([T|Q],[TC|QC]):- getCalqueLigne(T,TC),getCalque(Q,QC).

getCalqueLigne([],[]).
getCalqueLigne([x|QL],[x|QCL]):-getCalqueLigne(QL,QCL),!.
getCalqueLigne([TL|QL],[y|QCL]):-getCalqueLigne(QL,QCL).

check_not_base(X,Y,B) :- \+get_coord_y(X,Y,B).

%%%%%%%%%%%%%%%%%%%%%%%%% ** Lecture user ** %%%%%%%%%%%%%%%%%%%%%%%%%%%

messageErrorOption:-  write('** Erreur dans le choix d\'option **').

read_coord_user(X,Y):- nl,write('Ligne de la case : '),nl,write(' ---->'), read(X),read_valide(X),nl,
					   write('Colonne de la case : '),nl,write(' ---->'), read(Y),read_valide(Y),nl,!.

read_coord_user(_,_):- nl,write('***** COORDONNEES INVALIDES *****'),nl,fail.

read_valeur(V):- write('valeur de la case : '),nl,write(' ---->'), read(V),read_valide(V),nl,!.

read_valeur(V):- nl,write('***** VALEUR INVALIDE *****'),nl,fail.

read_valide(X):- X>0,X<10.

read_nb_cases(NB) :- write('nombre de cases au plus à changer : '),nl,write(' ---->'), read(NB),nl,!.


	
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

calque([
[x,x,x,x,x,y,x,x,x],
[x,x,x,x,y,x,x,x,x],
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

solvedEx([[1,4,3,9,8,6,2,5,7],
[6,7,9,4,2,5,3,8,1],
[2,8,5,7,3,1,6,9,4],
[9,6,2,3,5,4,1,7,8],
[3,5,7,6,1,8,9,4,2],
[4,1,8,2,7,9,5,6,3],
[8,2,1,5,6,7,4,3,9],
[7,9,6,1,4,3,8,2,5],
[5,3,4,8,9,2,7,1,x]]).

