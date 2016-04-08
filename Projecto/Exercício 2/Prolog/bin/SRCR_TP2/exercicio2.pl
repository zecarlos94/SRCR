%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SIST. REPR. CONHECIMENTO E RACIOCINIO - MiEI/3

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Programacao em logica estendida

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: Declaracoes iniciais

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% SICStus PROLOG: definicoes iniciais

:- dynamic '-'/1.
:- dynamic mamifero/1.
:- dynamic morcego/1.
%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado voa: N -> {V,F,D}

voa( X ) :- ave( X ) , nao( excecao( voa( X ) ) ).
-voa( tweety ).
-voa( X ) :- mamifero( X ) , nao( excecao( -voa( X ) ) ).
-voa( X ) :- excecao( voa( X ) ).
voa( X ) :- excecao( -voa( X ) ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado excecao: N -> {V,F,D}

excecao( voa( X ) ) :- avestruz( X ).
excecao( voa( X ) ) :- pinguim( X ).
excecao( -voa( X ) ) :- morcego( X ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado ave: Nome -> {V,F,D}

ave( pitigui ).

ave( X ) :- canario( X ).
ave( X ) :- avestruz( X ).
ave( X ) :- pinguim( X ).
ave( X ) :- piriquito( X ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
canario( piupiu ).
avestruz( trux ).
pinguim( pingu ).
cao( boby ).
morcego( batemene ).

%--------------------------------- - - - - - - - - - -  -  -  -  -   -
% Extensao do predicado mamifero: Nome -> {V,F,D}

mamifero( silvestre ).

mamifero( X ) :- cao( X ).
mamifero( X ) :- gato( X ).
mamifero( X ) :- morcego( X ).

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
