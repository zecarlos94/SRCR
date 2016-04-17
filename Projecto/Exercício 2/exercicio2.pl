% Caracaterização das funções ---------------------------------------------------------------------------------------------------------------------

% utente: Id_Utente, Nome, Idade, Morada -> {V, F, D}
% servico: Id_Serviço, Descrição, Instituição, Cidade -> {V, F, D}
% consulta: Data, Id_Utente, Id_Serviço, Custo -> {V, F, D}

% nao: Predicado -> {V, F}
% demo: Predicado, Resposta -> {V, F}
% inserir: Predicado -> {V, F}
% remover: Predicado -> {V, F}
% testar: Predicado -> {V, F}
% removeNulo: Predicado -> {V, F}
% registar: Predicado -> {V, F}
% eliminar: Predicado -> {V, F}
% listagem: Predicado, Lista de Ocorrências -> {V, F}
% tamanho: Lista de Valores, Tamanho da Lista -> {V, F}
% soma: Lista de Valores, Soma dos Valores -> {V, F}
% media: Lista de Valores, Média dos Valores -> {V, F}
% filtraConsultas: Lista de Duplos (Data, Custo), Lista de Datas, Total a Pagar -> {V, F}
% e: Resposta 1, Resposta 2, Resposta Final -> {V, F}
% demoExtendido: Questao, Resposta -> {V, F}

% exception: Predicado -> {V, F}
% nulo: Valor -> {V, F}

% mediaIdades: Resultado -> {V, F}
% servicosInstituicao: Id Instituição, Lista de Serviços -> {V, F}
% getConsultas: Id Utente, Lista de Datas, Total a Pagar -> {V, F}


% Definições iniciais -----------------------------------------------------------------------------------------------------------------------------

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).

:- dynamic utente/4.
:- dynamic servico/4.
:- dynamic consulta/4.
:- dynamic '-'/1.

:- op(900, xfy, '::').

:- op(900, xfy, 'e'). % é para o demoExtendido funcionar

% Funções auxiliares -----------------------------------------------------------------------------------------------------------------------------------

nao(Q) :- Q, !, fail.
nao(Q).

demo(Q, verdadeiro) :- Q.
demo(Q, falso) :- -Q.
demo(Q, desconhecido) :- nao(Q), nao(-Q).

inserir(T) :- assert(T).
inserir(T) :- retract(T), !, fail.

remover(T) :- retract(T).
remover(T) :- assert(T), !, fail.

testar([]).
testar([I | L]) :- I, testar(L).

removeNulo(utente(IdU, N, I, M)) :- I \= xpto1, retract(utente(IdU, N, xpto1, M)).
removeNulo(utente(IdU, N, I, M)) :- M \= xpto2, retract(utente(IdU, N, I, xpto2)).
removeNulo(servico(IdS, D, I, C)) :- I \= xpto3, retract(servico(IdS, D, xpto3, C)).
removeNulo(servico(IdS, D, I, C)) :- C \= xpto4, retract(servico(IdS, D, I, xpto4)).
removeNulo(consulta(D, IdU, IdS, C)) :- D \= xpto5, retract(consulta(xpto5, IdU, IdS, C)).
removeNulo(consulta(D, IdU, IdS, C)) :- C \= xpto6, retract(consulta(D, IdU, IdS, xpto6)).

registar(T) :- removeNulo(T), registar(T).
registar(T) :- findall(I, +T :: I, L), inserir(T), testar(L).

eliminar(T) :- findall(I, -T :: I, L), remover(T), testar(L).

listagem(utente, S) :- findall(utente(IdU, N, I, M), utente(IdU, N, I, M), S).
listagem(servico, S) :- findall(servico(IdS, D, I, C), servico(IdS, D, I, C), S).
listagem(consulta, S) :- findall(consulta(D, IdU, IdS, C), consulta(D, IdU, IdS, C), S).

tamanho([], 0).
tamanho([X | T], R) :- X \= xpto1, tamanho(T, R2), R is 1 + R2.
tamanho([X | T], R) :- X == xpto1, tamanho(T, R).

soma([], 0).
soma([X | T], R) :- X \= xpto1, soma(T, R2), R is X + R2.
soma([X | T], R) :- X == xpto1, soma(T, R).

media([], 0).
media(L, R) :- soma(L, S), tamanho(L, T), R is S / T.

filtraConsultas([], [], 0).
filtraConsultas([(D, C) | T], [D | Rd], Tp) :- nao(nulo(C)), nao(nulo(D)), filtraConsultas(T, Rd, Tp2), Tp is C + Tp2.
filtraConsultas([(D, C) | T], Rd, Tp) :- nulo(C), filtraConsultas(T, Rd, Tp).
filtraConsultas([(D, C) | T], Rd, Tp) :- nulo(D), filtraConsultas(T, Rd, Tp).

e(verdadeiro,verdadeiro,verdadeiro).
e(verdadeiro,falso,falso).
e(verdadeiro,desconhecido,desconhecido).

e(falso,verdadeiro,falso).
e(falso,falso,falso).
e(falso,desconhecido,falso).

e(desconhecido,verdadeiro,desconhecido).
e(desconhecido,falso,falso).
e(desconhecido,desconhecido,desconhecido).

demoExtendido(Q1 e Q2, R) :- demo(Q1, R1), demoExtendido(Q2, R2), e(R1, R2 ,R).
demoExtendido(Q1, R1) :- demo(Q1, R1).

% Base de conhecimento de utentes ----------------------------------------------------------------------------------------------------------------------

-utente(IdU, N, I, M) :- nao(utente(IdU, N, I, M)), nao(exception(utente(IdU, N, I, M))).


% Conhecimento perfeito
utente(1, manuel_faria, 24, rua_das_papoilas).
utente(2, carlos_sousa, 45, rua_dos_malmequeres).


% Conhecimento imperfeito incerto
utente(3, joao_seabra, xpto1, rua_da_alegria).
utente(4, tiago_barbosa, 37, xpto2).

exception(utente(Id, N, I, M)) :- utente(Id, N, xpto1, M).
exception(utente(Id, N, I, M)) :- utente(Id, N, I, xpto2).

nulo(xpto1).
nulo(xpto2).


% Conhecimento imperfeito impreciso
-utente(5, mario_cardoso, 33, rua_das_tristezas).
exception(utente(5, mario_cardoso, 34, rua_das_tristezas)).
exception(utente(5, mario_cardoso, 35, rua_das_tristezas)).

exception(utente(6, joel_vaz, 56, rua_das_estrelas)).
exception(utente(6, joel_vaz, 56, rua_das_luas)).


% Conhecimento imperfeito interdito
exception(utente(7, jose_esteves, I, rua_das_alcatifas)).
exception(utente(8, otavio_correia, 79, M)).

+utente(7, jose_esteves, I, rua_das_alcatifas) :: (findall((7, jose_esteves, I, rua_das_alcatifas), utente(7, jose_esteves, I, rua_das_alcatifas), R), length(R, T), T == 0).
+utente(8, otavio_correia, 79, M) :: (findall((8, otavio_correia, 79, M), utente(8, otavio_correia, 79, M), R), length(R, T), T == 0).


% Base de conhecimento de serviços ------------------------------------------------------------------------------------------------------------------

-servico(IdS, D, I, C) :- nao(servico(IdS, D, I, C)), nao(exception(servico(IdS, D, I, C))).


% Conhecimento perfeito
servico(1, oncologia, hospital_braga, braga).
servico(2, pediatria, hospital_porto, porto).


% Conhecimento imperfeito incerto
servico(3, cirurgia, xpto3, lisboa).
servico(4, radiologia, ipo_porto, xpto4).

exception(servico(IdS, D, I, C)) :- servico(IdS, D, xpto3, C).
exception(servico(IdS, D, I, C)) :- servico(IdS, D, I, xpto4).

nulo(xpto3).
nulo(xpto4).


% Conhecimento imperfeito impreciso

exception(servico(5, cardiologia, hospital_faro, faro)).
exception(servico(5, urologia, hospital_faro, faro)).

-servico(6, psiquiatria, hospital_militar, braga).
exception(servico(6, psiquiatria, hospital_militar, lisboa)).
exception(servico(6, psiquiatria, hospital_militar, porto)).


% Conhecimento imperfeito interdito

exception(servico(7, psicologia, hospital_amadora_sintra, C)).
exception(servico(IdS, oftalmologia, hospital_sao_joao, porto)).

+servico(7, psicologia, hospital_amadora_sintra, C) :: (findall((7, psicologia, hospital_amadora_sintra, C), servico(7, psicologia, hospital_amadora_sintra, C), S), length(S, T), T == 0).
+servico(IdS, oftalmologia, hospital_sao_joao, porto) :: (findall((IdS, oftalmologia, hospital_sao_joao, porto), servico(IdS, oftalmologia, hospital_sao_joao, porto), S), length(S, T), T == 0).


% Base de conhecimento de consultas ---------------------------------------------------------------------------------------------------------------

-consulta(D, IdU, IdS, C) :- nao(consulta(D, IdU, IdS, C)), nao(exception(consulta(D, IdU, IdS, C))).


% Conhecimento perfeito
consulta(18-4-2015, 1, 1, 25).
consulta(25-2-2016, 2, 3, 30).


% Conhecimento imperfeito incerto
consulta(xpto5, 3, 1, 25).
consulta(30-5-2014, 4, 2, xpto6).

nulo(xpto5).
nulo(xpto6).

exception(consulta(D, IdU, IdS, C)) :- consulta(xpto5, IdU, IdS, C).
exception(consulta(D, IdU, IdS, C)) :- consulta(D, IdU, IdS, xpto6).


% Conhecimento imperfeito impreciso
-consulta(22-7-2015, 5, 5, 25).
exception(consulta(20-7-2015, 5, 5, 25)).
exception(consulta(21-7-2015, 5, 5, 25)).

exception(consulta(9-9-2013, 6, 2, 30)).
exception(consulta(9-9-2013, 6, 2, 20)).


% Conhecimento imperfeito interdito
exception(consulta(7-12-2015, 4, 3, C)).
exception(consulta(D, 1, 4, 10)).

+consulta(7-12-2015, 4, 3, C) :: (findall((7-12-2015, 4, 3, C), consulta(7-12-2015, 4, 3, C), S), length(S, T), T == 0).
+consulta(D, 1, 4, 10) :: (findall((D, 1, 4, 10), consulta(D, 1, 4, 10), S), length(S, T), T == 0).


% Não pode inserir utentes com o mesmo id --------------------------------------------------------------------------------------------------------

+utente(IdU, N, I, M) :: (findall((IdU, N2, I2, M2), utente(IdU, N2, I2, M2), S), length(S, T), T == 1).


% Não pode inserir serviços com o mesmo id -------------------------------------------------------------------------------------------------------

+servico(IdS, D, I, C) :: (findall((IdS, D2, I2, C2), utente(Id, D2, I2, C2), S), length(S, T), T == 1).


% Não pode inserir consultas do mesmo utente no mesmo serviço e na mesma data --------------------------------------------------------------------

+consulta(D, IdU, IdS, C) :: (findall((D, IdU, IdS, C2), consulta(D, IdU, IdS, C2), S), length(S, T), T == 1).


% Para inserir uma consulta deverá já existir o utente -------------------------------------------------------------------------------------------

+consulta(D, IdU, IdS, C) :: (findall(IdU, utente(IdU, N, I, M), S), length(S, T), T == 1).


% Para inserir uma consulta deverá já existir o serviço -------------------------------------------------------------------------------------------

+consulta(D, IdU, IdS, C) :: (findall(IdS, servico(IdS, Descr, I, Cid), S), length(S, T), T == 1).


% Para remover um serviço, este não deve ter consultas associadas ---------------------------------------------------------------------------------

-servico(IdS, D, I, C) :: (findall(IdS, consulta(Data, IdU, IdS, Custo), S), length(S, T), T == 0).


% Para remover um utente, este não deve ter consultas associadas ----------------------------------------------------------------------------------

-utente(IdU, N, I, M) :: (findall(IdU, consulta(Data, IdU, IdS, Custo), S), length(S, T), T == 0).


% Não é permitido adicionar exceções repetidas ----------------------------------------------------------------------------------------------------

+exception(Q) :: (findall(Q, exception(Q), S), length(S, T), T == 1).


% Queries ------------------------------------------------------------------------------------------------------------------------------

% Calcula a média de idades dos utentes (apenas daqueles que possuem uma informação certa sobre a idade)
mediaIdades(R) :- findall(I, utente(IdU, N, I, M), S), media(S, R).

% Devolve a lista de serviços de uma instituição
servicosInstituicao(I, R) :- findall((IdS, D), servico(IdS, D, I, C), R).

% Dado um número de utente, devolve as datas de todas as consultas do mesmo e o total a pagar (desde que as datas sejam conhecidas)
getConsultas(IdU, Lc, Tp) :- findall((D, C), consulta(D, IdU, IdS, C), S), filtraConsultas(S, Lc, Tp).
