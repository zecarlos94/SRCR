%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


:- op(900,xfy,'::').
:- dynamic arbitro/1.


%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado demo: Questao,Resposta -> {V,F}

demo( Questao,verdadeiro ) :-
    Questao.
demo( Questao,falso ) :-
    -Questao.
demo( Questao,desconhecido ) :-
    nao( Questao ),
    nao( -Questao ).




%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do meta-predicado nao: Questao -> {V,F}

nao( Questao ) :-
    Questao, !, fail.
nao( Questao ).

-jogo(A, B, C):-
	nao(jogo(A,B,C)),
	nao( excecao( jogo(A, B, C) ) ).

arbitro(almeida_antunes).
arbitro(baltazar_borges).
arbitro(costa_carvalho).
arbitro(duarte_durao).
arbitro(edgar_esteves).
arbitro(francisco_franca).
arbitro(guerra_godinho).
arbitro(helder_heitor).
arbitro(ivo_inocencio).

%--------------- jogo 1 -------------------------------
jogo(1,almeida_antunes, 500).

%------------------- jogo 2 ---------------------------
jogo(2,baltazar_borges,xpto023).
excecao( jogo( J, A, V) ):-
	jogo( J, A, xpto023).

%--------------------- jogo 3 -------------------------


excecao( arbitro( 3, costa_carvalho, 500) ).
excecao( arbitro( 3, costa_carvalho, 2500) ).

%----------------------- jogo 4 -----------------------
excecao(jogo(4,duarte_durao,B)):- B=<750,
	B>= 250.


%--------------------- jogo 5 -----------------------
jogo(5,edgar_esteves,xpto723).

excecao( arbitro(J,A,V) ):-
	arbitro(J, A, xpto723).

nulo(xpto723).

+jogo( J, A, V ) :: (findall( (Js,A,Vs),(jogo(Js,edgar_esteves,Vs),nao(nulo(Vs))),S ),
                length( S,N ), N == 0
                  ).

%----------------------------- jogo 6 --------------------

jogo(6,francisco_franca,250).
excecao(jogo(6,francisco_franca,B)):-B>5000.




%----------------- jogo 7 ----------------------
-jogo(7,guerra_godinho,2500).
jogo(7, guerra_godinho,xpto523).
excecao( jogo( J, A ,V ) ) :-
	jogo( J, A , xpto523).


%%------------------------ jogo 8 ------------------------
excecao(jogo(8,helder_heitor,V)) :- cerca(1000).

cerca(V) :- V >= V * 0.75,
	V =< 1.25.

%-------------------------- jogo 9 -------------------------
excecao(jogo(9,ivo_inocencio,V)):- mtproximo(3000).


mtproximo(V):- V =< V * 1.1,
	V >= V * 0.9.
