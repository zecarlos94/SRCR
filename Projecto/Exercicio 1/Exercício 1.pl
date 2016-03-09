% Caracterização das funções ---------------------------------------------------------------------------------------------------

% utente: Nome, Serviço, Profissional, Instituição -> {V, F}
% servico: Designação, Instituição -> {V, F}
% profissional: Nome, Serviço, Instituição -> {V, F}
% instituicao: Nome -> {V, F}
% negate: Questão -> {V, F}
% pertence: Elemento, Lista -> {V, F}
% concatenar: Lista 1, Lista 2, Resultado -> {V, F}
% apagar: Elemento, Lista, Resultado -> {V, F}
% remover: Lista 1, Lista 2, Resultado -> {V, F}
% tira_repetidos: Lista, Resultado -> {V, F}
% inserir: Questão -> {V, F}
% testar: Lista -> {V, F}
% servicos_instituicao: Instituição, Lista de Serviços -> {V, F}
% utentes_instituicao: Instituição, Lista de Utentes -> {V, F}
% utentes_servico: Serviço, Lista de Utentes -> {V, F}
% utentes_servico: Serviço, Instituição, Lista de Utentes -> {V, F}
% instituicoes_servico: Serviço, Lista de Instituições -> {V, F}
% instituicoes_servicos: Lista de Serviços, Lista de Instituições -> {V, F}
% nao_servicos_instituicao: Instituição, Lista de Serviços -> {V, F}
% instituicoes_profissional: Profissional, Lista de Instituições -> {V, F}
% info_utente: Utente, Tipo, Lista -> {V, F}
% registar: Questão -> {V, F}
% remover: Questão -> {V, F}


% Declarações iniciais ---------------------------------------------------------------------------------------------------------

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


% Definições iniciais ----------------------------------------------------------------------------------------------------------

:- op(900, xfy, '::').
:- dynamic utente/4.
:- dynamic servico/2.
:- dynamic profissional/3.
:- dynamic instituicao/1.


% Base de Conhecimento sobre Utentes -------------------------------------------------------------------------------------------

utente(jose_esteves, oncologia, antonio_abreu, ipo_porto).
utente(miguel_silva, clinica_geral, manuel_pereira, hospital_braga).
utente(carlos_sousa, cirurgia, pedro_soares, hospital_lisboa).
utente(samuel_cunha, cirurgia, joao_pereira, hospital_braga).
utente(janeiro_fevereiro, psiquiatria, gru_maldisposto, hospital_braga).


% Base de Conhecimento sobre Serviços ------------------------------------------------------------------------------------------

servico(oncologia, ipo_porto).
servico(cirurgia, hospital_braga).
servico(clinica_geral, hospital_braga).
servico(cirurgia, hospital_lisboa).
servico(psiquiatria, hospital_braga).


% Base de Conhecimento sobre Profissionais -------------------------------------------------------------------------------------

profissional(antonio_abreu, oncologia, ipo_porto).
profissional(manuel_pereira, clinica_geral, hospital_braga).
profissional(pedro_soares, cirurgia, hospital_lisboa).
profissional(joao_pereira, cirurgia, hospital_braga).
profissional(gru_maldisposto, psiquiatria, hospital_braga).


% Base de Conhecimento sobre Instituições --------------------------------------------------------------------------------------

instituicao(hospital_braga).
instituicao(hospital_lisboa).
instituicao(ipo_porto).


% Funções Auxiliares -----------------------------------------------------------------------------------------------------------

% Nega uma função
negate(A) :- A, !, fail.
negate(A).

% Verifica se um elemento pertence a uma lista
pertence(X, [X | T]).
pertence(X, [H | T]) :- pertence(X, T).

% Concatena duas listas, sem que haja repetidos
concatenar(L, [], L).
concatenar([], L, L).
concatenar([H | T], L, R) :- pertence(H, L), concatenar(T, L, R).
concatenar([H | T], L, [H | R]) :- negate(pertence(H, L)), concatenar(T, L, R).

% Apaga um elemento de uma lista
apagar(X, [], []).
apagar(X, [X | T], T1) :- apagar(X, T, T1).
apagar(X, [H | T], [H | R]) :- apagar(X, T, R).

% Remove todos os elementos de uma lista, de uma outra
remover([], S, S).
remover([X | L], S, R) :- apagar(X, S, R1), remover(L, R1, R).

% Elimina os repetidos de uma lista
tira_repetidos([], []).
tira_repetidos([H | T], [H | R]) :- negate(pertence(H, T)), tira_repetidos(T, R).
tira_repetidos([H | T], R) :- pertence(H, T), tira_repetidos(T, R).

% Insere informação na base de conhecimento
inserir(T) :- assert(T).
inserir(T) :- retract(T), !, fail.

% Testa se todos os invariantes são verificados
testar([]).
testar([I | L]) :- I, testar(L).


% Queries ----------------------------------------------------------------------------------------------------------------------

% Identificar os serviços existentes numa instituição 
servicos_instituicao(I, S) :- findall(X, servico(X, I), S).

% Identificar os utentes de uma instituição
utentes_instituicao(I, U) :- findall(N, utente(N, S, P, I), U).

% Identificar os utentes de um determinado serviço
utentes_servico(S, U) :- findall(N, utente(N, S, P, I), U).

% Identificar os utentes de um determinado serviço numa instituição
utentes_servico(S, I, U) :- findall(N, utente(N, S, P, I), U).

% Identificar as instituições onde seja prestado um serviço
instituicoes_servico(S, I) :- findall(N, servico(S, N), I).

% Identificar as instituições onde seja prestado um conjunto de serviços
instituicoes_servicos([], []).
instituicoes_servicos([S | T], I) :- findall(N, servico(S, N), Li), instituicoes_servicos(T, Lt), concatenar(Li, Lt, I).

% Identificar os serviços que não se podem encontrar numa instituição
nao_servicos_instituicao(I, S) :- findall(X, servico(X, Y), L1), findall(X, servico(X, I), L2), remover(L2, L1, R), tira_repetidos(R, S).

% Determinar as instituições onde um profissional presta serviço
instituicoes_profissional(P, I) :- findall(N, profissional(P, S, N), I).

% Determinar todas as instituições (ou serviços, ou profissionais) a que um utente já recorreu
info_utente(U, instituicoes, L) :- findall(I, utente(U, S, P, I), L).
info_utente(U, servicos, L) :- findall(S, utente(U, S, P, I), L).
info_utente(U, profissionais, L) :- findall(P, utente(U, S, P, I), L).

% Registar utentes, profissionais, serviços ou instituições
registar(T) :- findall(I, +T :: I, L), inserir(T), testar(L).

% Remover utentes (ou profissionais, ou serviços, ou instituições) dos registos
% Under Construction...


% Invariantes para Utentes --------------------------------------------------------------------------------------------------------

% Não podem existir utentes repetidos na mesma instituição com serviços e profissionais repetidos
+utente(U, S, P, I) :: (findall((U, S, P, I), utente(U, S, P, I), L), length(L, R), R == 1).

% Só pode inserir um utente se existir a instituição
+utente(U, S, P, I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).

% Só pode inserir um utente se existir o serviço na dada instituição
+utente(U, S, P, I) :: (findall((S, I), servico(S, I), L), length(L, R), R == 1).

% Só pode inserir um utente se existir o profissional do serviço na instituição
+utente(U, S, P, I) :: (findall((P, S, I), profissional(P, S, I), L), length(L, R), R == 1).


% Invariantes para Serviços -------------------------------------------------------------------------------------------------------

% Não podem existir serviços repetidos na mesma instituição
+servico(S, I) :: (findall((S, I), servico(S, I), L), length(L, R), R == 1).

% Só pode inserir um serviço numa instituição se esta existir
+servico(S, I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).


% Invariantes para Profissionais --------------------------------------------------------------------------------------------------

% Não podem existir profissionais repetidos na mesma instituição com o mesmo serviço
+profissional(P, S, I) :: (findall((P, S, I), profissional(P, S, I), L), length(L, R), R == 1).

% Só pode inserir um profissional numa instituição se esta existir
+profissional(P, S, I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).

% Só pode inserir um profissional de um determinado serviço numa instituição se esse serviço existir na mesma
+profissional(P, S, I) :: (findall((S, I), servico(S, I), L), length(L, R), R == 1).


% Invariantes para Instituições ---------------------------------------------------------------------------------------------------

% Não podem existir instituições repetidas
+instituicao(I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).

