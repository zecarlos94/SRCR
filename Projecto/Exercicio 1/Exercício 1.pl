% Caracterização das funções ---------------------------------------------------------------------------------------------------

% ficha_utente: Nome Utente, Serviço, Profissional, Instituição -> {V, F}
% registo_utente: Nome Utente, Idade, Localidade -> {V, F}
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
% retirar: Questão -> {V, F}
% testar: Lista -> {V, F}
% comprimento: Lista, Tamanho -> {V, F}
% soma: Lista, Soma -> {V, F}
% media: Lista, Média -> {V, F}
% maior: (X, Y), (A, B), Resultado -> {V, F}
% menor: (X, Y), (A, B), Resultado -> {V, F}
% minIdade: Lista de Utentes, (Nome do Utente, Idade) -> {V, F}
% maxIdade: Lista de Utentes, (Nome do Utente, Idade) -> {V, F}
% repetidos: Lista 1, Lista 2, Resultado -> {V, F}
% listaUtentes: Resultado -> {V, F}
% nUtentesInst: Lista de Instituições, Resultado -> {V, F}
% nUtentesServ: Lista de Serviços, Resultado -> {V, F}
% nUtentesProf: Lista de Profissionais, Resultado -> {V, F}
% maxUtentes: Lista, Resultado -> {V, F}
% listaInstituicoes: Resultado -> {V, F}
% listaServicos: Resultado -> {V, F}
% listaProfissionais: Resultado -> {V, F}
% tiraIdades: Lista de Tuplos, Lista de Elementos, Resultado -> {V, F}

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
% profissionais_instituicao: Instituição, Lista de Profissionais -> {V,F}
% mediaIdades: Resultado -> {V, F}
% utentes_localidade: Localidade, Lista de Utentes -> {V, F}
% utentes_idade: Idade, Lista de Utentes -> {V, F}
% mais_idoso: (Nome do Utente, Idade) -> {V, F}
% mais_jovem: (Nome do Utente, Idade) -> {V, F}
% mediaIdades_utentes_instituicao: Instituição, Resultado -> {V, F}
% mediaIdades_utentes_servico: Serviço, Resultado -> {V, F}
% mediaIdades_utentes_profissional: Profissional, Resultado -> {V, F}
% instituicaoMaisUtentes: Resultado -> {V, F}
% servicoMaisUtentes: Resultado -> {V, F}
% profissionalMaisUtentes: Resultado -> {V, F}


% Declarações iniciais ---------------------------------------------------------------------------------------------------------

:- set_prolog_flag( discontiguous_warnings,off ).
:- set_prolog_flag( single_var_warnings,off ).
:- set_prolog_flag( unknown,fail ).


% Definições iniciais ----------------------------------------------------------------------------------------------------------

:- op(900, xfy, '::').
:- dynamic registo_utente/3.
:- dynamic ficha_utente/4.
:- dynamic servico/2.
:- dynamic profissional/3.
:- dynamic instituicao/1.


% Base de Conhecimento sobre Utentes -------------------------------------------------------------------------------------------

registo_utente(jose_esteves, 45, matosinhos).
registo_utente(miguel_silva, 13, gualtar).
registo_utente(carlos_sousa, 25, oeiras).
registo_utente(samuel_cunha, 39, esposende).
registo_utente(joana_fernandes, 72, lomar).

ficha_utente(jose_esteves, oncologia, antonio_abreu, ipo_porto).
ficha_utente(miguel_silva, clinica_geral, manuel_pereira, hospital_braga).
ficha_utente(carlos_sousa, cirurgia, pedro_soares, hospital_lisboa).
ficha_utente(samuel_cunha, cirurgia, joao_pereira, hospital_braga).
ficha_utente(joana_fernandes, psiquiatria, duarte_gomes, hospital_braga).


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
profissional(duarte_gomes, psiquiatria, hospital_braga).


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

% Remove informação da base de conhecimento
retirar(T) :- retract(T).
retirar(T) :- assert(T), !, fail.

% Testa se todos os invariantes são verificados
testar([]).
testar([I | L]) :- I, testar(L).

% Calcula comprimento de uma lista
comprimento([], 0).
comprimento([H | T], C1) :- comprimento(T, C2) , C1 is 1+C2.

% Calcula a soma de todos os valores de uma lista
soma([], 0).
soma([H | T], S1) :- soma(T, S2) , S1 is H+S2.

% Calcula a média de idades
media(I, R) :- soma(I, S), comprimento(I, L), R is S/L.

% De entre dois utentes, determina aquele que tem menor idade
menor((A, Ia), (B, Ib), (A, Ia)) :- Ia < Ib.
menor((A, Ia), (B, Ib), (B, Ib)) :- Ia >= Ib.

% De entre dois utentes, determina aquele que tem menor idade
maior((A, Ia), (B, Ib), (A, Ia)) :- Ia > Ib.
maior((A, Ia), (B, Ib), (B, Ib)) :- Ia =< Ib.

% Numa lista de utentes, determina aquele que tem menor idade
minIdade([H], H).
minIdade([H | T], H) :- minIdade(T, X), menor(H, X, H).
minIdade([H | T], X) :- minIdade(T, X), menor(H, X, X).

% Numa lista de utentes, determina aquele que tem menor idade
maxIdade([H], H).
maxIdade([H | T], H) :- maxIdade(T, X), maior(H, X, H).
maxIdade([H | T], X) :- maxIdade(T, X), maior(H, X, X).

% Devolve a lista com os elementos que se repetem nas duas listas
repetidos([], L, []).
repetidos(L, [], []).
repetidos([H | T], L, [H | R]) :- pertence(H, L), repetidos(T, L, R).
repetidos([H | T], L, R) :- negate(pertence(H, L)), repetidos(T, L, R).

% Devolve a listagem de todos os utentes na forma de tuplos (Utente, Idade)
listaUtentes(R) :- findall((U, I), registo_utente(U, I, L), R).

% Devolve um tuplo com o nome da instituição e o número de utentes da mesma
nUtentesInst([], []).
nUtentesInst([I | T], [(I, Nu) | R]) :- utentes_instituicao(I, U), comprimento(U, Nu), nUtentesInst(T, R).

% Devolve um tuplo com o nome do serviço e o número de utentes do mesmo
nUtentesServ([], []).
nUtentesServ([S | T], [(S, Nu) | R]) :- utentes_servico(S, U), comprimento(U, Nu), nUtentesServ(T, R).

% Devolve um tuplo com o nome do profissional e o número de utentes do mesmo
nUtentesProf([], []).
nUtentesProf([P | T], [(P, Nu) | R]) :- utentes_profissional(P, U), comprimento(U, Nu), nUtentesProf(T, R).

% Determina a instituição, o serviço ou o profissional com mais utentes
maxUtentes([H], H).
maxUtentes([H | T], H) :- maxUtentes(T, X), maior(H, X, H).
maxUtentes([H | T], X) :- maxUtentes(T, X), maior(H, X, X).

% Devolve a lista das instituições
listaInstituicoes(R) :- findall(I, instituicao(I), R).

% Devolve a lista dos serviços
listaServicos(L) :- findall(S, servico(S, I), R), tira_repetidos(R, L).

% Devolve a lista de profissionais
listaProfissionais(L) :- findall(P, profissional(P, S, I), R), tira_repetidos(R, L).

% Retira as idades dos tuplos e constrói uma lista com as mesmas, desde que U pertença a L
tiraIdades([], L, []).
tiraIdades([(U, I) | T], L, [I | R]) :- pertence(U, L), tiraIdades(T, L, R).
tiraIdades([(U, I) | T], L, R) :- negate(pertence(U, L)), tiraIdades(T, L, R).


% Queries ----------------------------------------------------------------------------------------------------------------------

% Identificar os serviços existentes numa instituição
servicos_instituicao(I, S) :- findall(X, servico(X, I), S).

% Identificar os utentes de uma instituição
utentes_instituicao(I, R) :- findall(U, ficha_utente(U, S, P, I), R).

% Identificar os utentes de um determinado serviço
utentes_servico(S, R) :- findall(U, ficha_utente(U, S, P, I), R).

% Identificar os utentes de um determinado serviço numa instituição
utentes_servico(S, I, R) :- findall(U, ficha_utente(U, S, P, I), R).

% Identificar as instituições onde seja prestado um serviço
instituicoes_servico(S, I) :- findall(N, servico(S, N), I).

% Identificar as instituições onde seja prestado um conjunto de serviços (REVER)
instituicoes_servicos([], []).
instituicoes_servicos([S | T], I) :- findall(N, servico(S, N), Li), instituicoes_servicos(T, Lt), concatenar(Li, Lt, I).

% Identificar os serviços que não se podem encontrar numa instituição
nao_servicos_instituicao(I, S) :- findall(X, servico(X, Y), L1), findall(X, servico(X, I), L2), remover(L2, L1, R), tira_repetidos(R, S).

% Determinar as instituições onde um profissional presta serviço
instituicoes_profissional(P, I) :- findall(N, profissional(P, S, N), R), tira_repetidos(R, I).

% Determinar todas as instituições, serviços ou profissionais a que um utente já recorreu
info_utente(U, instituicoes, L) :- findall(I, ficha_utente(U, S, P, I), L).
info_utente(U, servicos, L) :- findall(S, ficha_utente(U, S, P, I), L).
info_utente(U, profissionais, L) :- findall(P, ficha_utente(U, S, P, I), L).

% Registar utentes, profissionais, serviços ou instituições
registar(T) :- findall(I, +T :: I, L), inserir(T), testar(L).

% Remover utentes, profissionais, serviços ou instituições dos registos
remover(Q) :- findall(I, -Q :: I, L), retirar(Q), testar(L).


% Queries Extra -------------------------------------------------------------------------------------------------------------------

% Determinar os profissionais que prestam serviços numa instituicao sem repetidos 
profissionais_instituicao(I, P) :- findall(N, profissional(N, S, I), L), tira_repetidos(L, P).

% Calcula a média de idades dos utentes registados
mediaIdades(R) :- findall(I, registo_utente(U, I, Loc), L), media(L, R).

% Determinar a lista de utentes por localidade
utentes_localidade(L, R) :- findall(U, registo_utente(U, I, L), R).

% Determinar os utentes que recorreram a um profissional sem repetidos
utentes_profissional(P, R) :- findall(U, ficha_utente(U, S, P, I), Raux), tira_repetidos(Raux, R).

% Determinar a lista de utentes de um profissional por localidade
utentes_localidade_profissional(L, P, R) :- findall(U, registo_utente(U, I, L), X1), utentes_profissional(P, X2), repetidos(X1, X2, R).

% Determinar a lista de utentes de um servico por localidade
utentes_localidade_servico(L, S, R) :- findall(U, registo_utente(U, I, L), X1), utentes_servico(S, X2), repetidos(X1, X2, R).

% Determina a lista de utentes por idade
utentes_idade(I, U) :- findall(U, registo_utente(U, I, L), U).

% De entre os utentes registados, determina o que tem maior idade
mais_idoso(J) :- findall((U, I), registo_utente(U, I, L), R), maxIdade(R, J).

% De entre os utentes registados, determina o que tem menor idade
mais_jovem(J) :- findall((U, I), registo_utente(U, I, L), R), minIdade(R, J).

% Devolve a média de idades dos utentes de uma instituição
mediaIdades_utentes_instituicao(I, R) :- utentes_instituicao(I, R1), listaUtentes(R2), tiraIdades(R2, R1, Li), media(Li, R).

% Devolve a média de idades dos utentes de um serviço
mediaIdades_utentes_servico(S, R) :- utentes_servico(S, R1), listaUtentes(R2), tiraIdades(R2, R1, Li), media(Li, R).

% Devolve a média de idades dos utentes de um profissional
mediaIdades_utentes_profissional(P, R) :- utentes_profissional(P, R1), listaUtentes(R2), tiraIdades(R2, R1, Li), media(Li, R).

% Determinar a instituição com mais utentes
instituicaoMaisUtentes(I) :- listaInstituicoes(Li), nUtentesInst(Li, R), maxUtentes(R, I).

% Determinar o serviço com mais utentes
servicoMaisUtentes(S) :- listaServicos(Ls), nUtentesServ(Ls, R), maxUtentes(R, S).

% Determinar o profissional com mais utentes
profissionalMaisUtentes(S) :- listaProfissionais(Ls), nUtentesProf(Ls, R), maxUtentes(R, S).


% Invariantes para Utentes --------------------------------------------------------------------------------------------------------

% Não podem existir registos de utentes repetidos na base de conhecimento
+registo_utente(U, I, L) :: (findall((U, I, L), registo_utente(U, I, L), R), length(R, T), T == 1).

% Não podem existir fichas de utentes repetidas na mesma instituição com serviços e profissionais repetidos
+ficha_utente(U, S, P, I) :: (findall((U, S, P, I), ficha_utente(U, S, P, I), L), length(L, R), R == 1).

% Só pode inserir uma ficha de utente se existir a instituição
+ficha_utente(U, S, P, I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).

% Só pode inserir uma ficha de utente se existir o serviço na dada instituição
+ficha_utente(U, S, P, I) :: (findall((S, I), servico(S, I), L), length(L, R), R == 1).

% Só pode inserir uma ficha de utente se existir o profissional do serviço na instituição
+ficha_utente(U, S, P, I) :: (findall((P, S, I), profissional(P, S, I), L), length(L, R), R == 1).

% Só pode inserir uma ficha de utente se este estiver registado na base de conhecimento
+ficha_utente(U, S, P, I) :: (findall(U, registo_utente(U, I, L), R), length(R, T), T == 1). 

% Para se remover um registo de utente, é preciso que este exista na base de conhecimento
-registo_utente(U, I, L) :: (findall((U, I, L), registo_utente(U, I, L), R), length(R, T), T == 0).

% Para se remover um registo de utente, não podem existir fichas a ele associadas
-registo_utente(U, Idade, L) :: (findall(U, ficha_utente(U, S, P, Inst), R), length(R, T), T == 0).

% Para se remover uma ficha de utente, é preciso que esta exista na base de conhecimento
-ficha_utente(U, S, P, I) :: (findall((U, S, P, I), ficha_utente(U, S, P, I), L), length(L, R), R == 0).


% Invariantes para Serviços -------------------------------------------------------------------------------------------------------

% Não podem existir serviços repetidos na mesma instituição
+servico(S, I) :: (findall((S, I), servico(S, I), L), length(L, R), R == 1).

% Só pode inserir um serviço numa instituição se esta existir
+servico(S, I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).

% Para se remover um serviço, é preciso que este exista na base de conhecimento
-servico(S, I) :: (findall((S, I), servico(S, I), L), length(L, R), R == 0).

% Só pode remover um servico se não tiver utentes associados ao mesmo e na mesma instituição
-servico(S, I) :: (findall((U, S, I), ficha_utente(U, S, P, I), L), length(L, R), R == 0).

% Só pode remover um servico se não tiver profissionais associados ao mesmo e na mesma instituição
-servico(S, I) :: (findall((P, S, I), profissional(P, S, I), L), length(L, R), R == 0).


% Invariantes para Profissionais --------------------------------------------------------------------------------------------------

% Não podem existir profissionais repetidos na mesma instituição com o mesmo serviço
+profissional(P, S, I) :: (findall((P, S, I), profissional(P, S, I), L), length(L, R), R == 1).

% Só pode inserir um profissional numa instituição se esta existir
+profissional(P, S, I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).

% Só pode inserir um profissional de um determinado serviço numa instituição se esse serviço existir na mesma
+profissional(P, S, I) :: (findall((S, I), servico(S, I), L), length(L, R), R == 1).

% Para se remover um profissional, é preciso que este exista na base de conhecimento
-profissional(P, S, I) :: (findall((P, S, I), profissional(P, S, I), L), length(L, R), R == 0).

% Só pode remover um profissional se este não tiver utentes associados
-profissional(P, S, I) :: (findall((U, S, P, I), ficha_utente(U, S, P, I), L), length(L, R), R == 0).


% Invariantes para Instituições ---------------------------------------------------------------------------------------------------

% Não podem existir instituições repetidas
+instituicao(I) :: (findall(I, instituicao(I), L), length(L, R), R == 1).

% Para se remover uma instituição, é preciso que esta exista na base de conhecimento
-instituicao(I) :: (findall(I, instituicao(I), L), length(L, R), R == 0).

% Para se remover uma instituição, não podem existir utentes associados à mesma
-instituicao(I) :: (utentes_instituicao(I, U) , length(U, L), L == 0).

% Para se remover uma instituição, não podem existir serviços associados à mesma
-instituicao(I) :: (servicos_instituicao(I, S), length(S, L), L == 0).

% Para se remover uma instituição, não podem existir profissionais associados à mesma
-instituicao(I) :: (profissionais_instituicao(I, P), length(P, L), L == 0).
