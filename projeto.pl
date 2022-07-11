% Diogo Artur Rainha Lopes - 96732

:- [codigo_comum].

/*
combinacoes_soma(N, Els, Soma, Combs),
N eh um inteiro, 
Els eh uma lista de inteiros, 
Soma eh um inteiro, 
Combs eh a lista ordenada cujos elementos sao 
    as combinacoes N a N, dos elementos de Els cuja soma eh Soma.
*/
combinacoes_soma(N, Els, Soma, Combs) :-
    findall(Comb, combinacao(N, Els, Comb), L),
    setof(X, (member(X, L), sum_list(X, Soma)), Combs).

/*
permutacoes_soma(N, Els, Soma, Perms), 
N eh um inteiro, 
Els eh uma lista de inteiros, 
Soma eh um inteiro, 
Perms eh a lista ordenada cujos elementos sao as permutacoes das 
    combinacoes N a N, dos elementos de Els cuja soma eh Soma.
*/
permutacoes_soma(N, Els, Soma, Perms) :-
    findall(P, (combinacao(N, Els, Comb), permutation(Comb, P)), L),
    setof(X, (member(X, L), sum_list(X, Soma)), Perms).
    
/*
espaco_fila(Fila, Esp, H_V), em que Fila eh uma fila (linha ou coluna) de um puzzle
H_V eh um dos atomos 'h' ou 'v', conforme se trate de uma fila horizontal ou vertical, resp., 
Esp eh um espaco de Fila.
*/
espaco_fila(Fila, Esp, H_V) :-
    H_V == h,                                   % caso queiramos linhas
    espaco_aux(Fila, espaco(_, []), Esp, 1);
    H_V == v,                                   % caso queiramos colunas
    espaco_aux(Fila, espaco(_, []), Esp, 0).

% cond paragem, chega a uma posicao nao livre e percorreu pelo menos um espaco
espaco_aux([E | _], espaco(D, V), espaco(D, V), _) :-
    is_list(E),
    length(V, X),
    X >= 1.

% cond paragem, chegou ao fim da fila e percorreu pelo menos um espaco
espaco_aux([], espaco(D, V), espaco(D, V), _) :-
    length(V, K),
    K >= 1.

% seleciona caso seja um espaco e comeca a procura por posicoes livres
espaco_aux([E | T], espaco(_, _), Esp, I) :-
    is_list(E),                                 % espaco ocupado
    nth0(I, E, D),
    espaco_aux(T, espaco(D, []), Esp, I).

% vai adicionando posicoes livres
espaco_aux([E | T], espaco(D, V), Esp, I) :-
    \+(is_list(E)), !,                          % espaco livre
    append(V, [E], W),
    espaco_aux(T, espaco(D, W), Esp, I).

/*
espacos_fila(H_V, Fila, Espacos), em que Fila eh uma fila (linha ou coluna) de uma grelha 
H_V eh um dos atomos h ou v, 
Espacos eh a lista de todos os espacos de Fila, da esquerda para a direita.
*/
espacos_fila(H_V, Fila, Espacos):-
    bagof(Esp, espaco_fila(Fila, Esp, H_V), Espacos), !.
espacos_fila(_, _, []).                         % caso nao haja espacos, devolve vazio 


/*
espacos_puzzle(Puzzle, Espacos), em que Puzzle eh um puzzle, 
Espacos eh a lista de espacos de Puzzle.
*/
espacos_puzzle(Puzzle, Espacos) :- 
    maplist(espacos_fila(h), Puzzle, Esp_Linhas_Aux),       % analisa todo o puzzle na horizontal

	mat_transposta(Puzzle, Puzzle_T),
	maplist(espacos_fila(v), Puzzle_T, Esp_Colunas_Aux),    % e de seguida na vertical
	
    conc_aux(Esp_Linhas_Aux, Esp_Colunas_Aux, Espacos).
    %exclude(==([]), Esp_Aux, Espacos).              

conc_aux(Esp_Linhas_Aux, Esp_Colunas_Aux, Espacos) :-       % concatenar e retirar listas vazias
    append(Esp_Linhas_Aux, Espacos_Linhas),
    append(Esp_Colunas_Aux, Espacos_Colunas),
	append(Espacos_Linhas, Espacos_Colunas, Esp_Aux),
    exclude(==([]), Esp_Aux, Espacos). 
/*
espacos_com_posicoes_comuns(Espacos, Esp, Esps_com),
Espacos eh uma lista de espacos,
Esp eh um espaco, 
Esps_com eh a lista de espacos com variaveis em comum com Esp, exceptuando Esp. 
*/
pertence([H | _], El) :- H == El, !.   % auxiliar para verificar se um elemento pertence a uma lista
pertence([_ | T], El) :- pertence(T, El).

espacos_com_posicoes_comuns(Espacos, Esp, Esps_com) :-
    include(posicoes_comuns(Esp), Espacos, Espacos_Comuns),  % inclui espacos com variaveis em comum
    exclude(==(Esp), Espacos_Comuns, Esps_com).              % exclui o proprio

posicoes_comuns(espaco(_, E1), espaco(_, E2)) :-
    include(pertence(E1), E2, Espacos_Comuns),              % verifica uma variavel em comum entre
    Espacos_Comuns \== [].                                  % dois espacos 



/*
permutacoes_soma_espacos(Espacos, Perms_soma), em que Espacos eh uma lista de espacos, 
Perms_soma eh a lista de listas de 2 elementos, 
    em que o 1o elemento eh um espaco de Espacos e 
    o 2o eh a lista ordenada de permutacoes cuja soma eh igual a soma do espaco.
*/
permutacoes_soma_espacos(Espacos, Perms_soma) :-
    maplist(aux_perm(),Espacos, Perms_aux),                 
    maplist(aux_conc(), Espacos, Perms_aux, Perms_soma).    
    
aux_perm(espaco(D, E), P) :-            % auxiliar que verifica as permutacoes possiveis 
    length(E, LEN),                     % com o tamanho de cada permutacao
    findall(X, permutacoes_soma(LEN, [1, 2, 3, 4, 5, 6, 7, 8, 9], D, X), P).

aux_conc(Espaco, Permutacao, Final) :-  % auxiliar de concatenacao
    append([Espaco], Permutacao, Final).

/*
permutacao_possivel_espaco(Perm, Esp, Espacos, Perms_soma), em que Perm eh uma permutacao, Esp eh um 
espaco, Espacos eh uma lista de espacos, e Perms_soma eh uma lista de listas tal como obtida pelo 
predicado anterior, significa que Perm eh uma permutacao possivel para o espaco Esp.
*/
permutacao_possivel_espaco(Perm, espaco(D, P), Espacos, Perms_soma) :-
    nth0(X, Espacos, espaco(D, P)),         % indice de um espaco
    nth0(X, Perms_soma, A),                  % espaco com as suas permutacoes
    nth0(1, A, Perms),                        % permutacoes do espaco
    member(Perm, Perms),                       % para cada permutacao desse espaco
    Perm = P,                               % unifica
    espacos_com_posicoes_comuns(Espacos, espaco(D, P), Esps_com),
    verifica_perm(Esps_com, Perms_soma, Espacos).

verifica_perm([], _, _).
verifica_perm([H | T], Perms_soma, Espacos) :-  % auxiliar que percorre todos os espacos em comum
    nth0(X, Espacos, H),
    nth0(X, Perms_soma, A),
    nth0(1, A, Perms),              % permutacoes de um espaco com variavel em comum

    ver(H, Perms),                  % auxilar que verifica cada permutacao
    verifica_perm(T, Perms_soma, Espacos).


ver(espaco(_, E), Perms) :-         % verifica se ha permutacoes que sejam unificaveis
    member(P, Perms),               % tendo em conta a variavel ja preenchida do espaco inicial
    unificaveis(E, P), !.

unificaveis(X, Y) :- \+ (X \= Y).   % auxiliar que verifica a unificacao sem a executar

/*
permutacoes_possiveis_espaco(Espacos, Perms_soma, Esp,Perms_poss), 
Espacos eh uma lista de espacos,
Perms_soma eh uma lista de listas tal como obtida pelo predicado permutacoes_soma_espacos, 
Esp eh um espaco, 
Perms_poss eh uma lista de 2 elementos 
    em que o primeiro eh alista de variaveis de Esp 
    e o segundo eh a lista ordenada de permutacoes possiveis para o espaco Esp.
*/
permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(D, E), Perms_poss) :-
    findall(Perm, permutacao_possivel_espaco(Perm, espaco(D, E), Espacos, Perms_soma), L),
    sort(L, L2),                    
    append([E], [L2], Perms_poss).


/*
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps),
Espacos eh uma lista de espacos, 
Perms_poss_esps eh a lista com os espacos e com as permutacoes possiveis para ele.
*/
permutacoes_possiveis_espacos(Espacos, Perms_poss_esps) :-
    permutacoes_soma_espacos(Espacos, Perms_soma),          % permutacoes de cada espaco
    maplist(perms_poss_aux(Perms_soma, Espacos), Espacos, Perms_poss_esps).

perms_poss_aux(Perms_soma, Espacos, espaco(D, E), Perms_poss_esps) :-   
    sort([Perms_poss], L),
    append(L, Perms_poss_esps),
    permutacoes_possiveis_espaco(Espacos, Perms_soma, espaco(D, E), Perms_poss).

/*
numeros_comuns(Lst_Perms, Numeros_comuns),
Lst_Perms eh uma lista de permutacoes, 
Numeros_comuns eh uma lista de pares(pos, numero), 
    todas as listas de Lst_Perms contem  o  numero numero na  posicao pos.
*/
numeros_comuns(Lst_Perms, Numeros_comuns) :- 
    mat_transposta(Lst_Perms, Lst_Perms_T),      % todos os numeros com a mesma posicao numa lista
    length(Lst_Perms_T, Len),
    findall(X, between(1, Len, X), Posicoes),
    maplist(elementos_iguais, Posicoes, Lst_Perms_T, Numeros_comuns_aux),   % se todos os numeros na
    exclude(==([]), Numeros_comuns_aux, Numeros_comuns).                    % mesma pos forem iguais
                                                                            % entao e comum
elementos_iguais(Pos, Nums, Par) :-
    maplist(=(_), Nums), !,         % verifica se sao iguas em todas as posicoes
    nth0(0, Nums, Num),             % se forem, encontramos o primeiro
    Par = (Pos, Num).               % e adicionamos ao Par
elementos_iguais(_, _, []).


/*
atribui_comuns(Perms_Possiveis), 
Perms_Possiveis eh uma lista de permutacoes possiveis, 
    atribui_comuns actualiza esta lista atribuindo a cada espaco numeros comuns a todas 
    as permutacoes possiveis para esse espaco.
*/
atribui_comuns([]).
atribui_comuns([[Esp, Perms] | T_Perms]) :-  
    numeros_comuns(Perms, Numeros_com),
    atr(Esp, Numeros_com),
    atribui_comuns(T_Perms), !.

atr(_, []).
atr(Esp, [(Pos, Num) | T_Nums_com]) :-  % auxiliar para verificar se os elementos da lista sao
    nth1(Pos, Esp, Num),                % sao unificaveis com os nums em comum
    atr(Esp, T_Nums_com), !.

/*
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis), 
Perms_Possiveis eh uma lista de permutacoes possiveis, 
Novas_Perms_Possiveis eh o resultado de tirar permutacoes impossiveis de Perms_Possiveis.
*/
retira_impossiveis(Perms_Possiveis, Novas_Perms_Possiveis) :-
    maplist(retira_impossiveis_aux, Perms_Possiveis, Novas_Perms_Possiveis),!.

% retira permutacoes impossiveis das permutacoes, ou seja, as que nao sao unificaveis
retira_impossiveis_aux([Espaco, Perms], [Espaco, Perms_aux]) :-     
    bagof(Perm, (member(Perm, Perms), unificaveis(Espaco, Perm)), Perms_aux).   


/*
simplifica(Perms_Possiveis, Novas_Perms_Possiveis),
Perms_Possiveis eh uma lista de permutacoes possiveis, 
Novas_Perms_Possiveis eh o resultado de simplificar Perms_Possiveis.
*/
simplifica(Perms_Possiveis, Novas_Perms_Possiveis) :-
    atribui_comuns(Perms_Possiveis),                           % atribui-se comuns a todas as perms                 
    retira_impossiveis(Perms_Possiveis, Perms_Possiveis_aux),  % retira-se as impossiveis                   
    (Perms_Possiveis_aux \== Perms_Possiveis, simplifica(Perms_Possiveis_aux, Novas_Perms_Possiveis);
    (Novas_Perms_Possiveis = Perms_Possiveis_aux)).
    % caso ainda seja possivel simplificar, repete-se o processo

/*
inicializa(Puzzle, Perms_Possiveis), em que Puzzle eh um puzzle, significa que Perms_Possiveis
eh a lista de permutacoes possiveis simplificada para Puzzle.
*/
inicializa(Puzzle, Perms_Possiveis) :-
    espacos_puzzle(Puzzle, Espacos),
    permutacoes_possiveis_espacos(Espacos, Aux),
    simplifica(Aux, Perms_Possiveis).


/*
escolhe_menos_alternativas(Perms_Possiveis, Escolha),
Perms_Possiveis eh uma lista de permutacoes possiveis, 
Escolha eh o elemento de Perms_Possiveis escolhido, 
    sendo este o espaco com o menor numero de permutacoes e mais que apenas uma. 
Se todos os espacos em Perms_Possiveis tiverem associadas listas de permutacoes unitarias, 
    o predicado devolve false.
*/
retira_uni([_, P]) :-   % auxiliar que verifica se sao permutacoes unitarias
    length(P, K),
    K > 1.

min([X], X) :- !.       % auxiliar que retorna o menor valor de uma lista 
min([A ,B | T], Min):-
    (A > B, min([B | T], Min);
    (min([A | T], Min))).


escolhe_menos_alternativas(Perms_Possiveis, Escolha) :-
    include(retira_uni, Perms_Possiveis, Possiveis_aux),  % lista de perms nao unitarias
    maplist(nth0(1), Possiveis_aux, Perms),               % lista de permutacoes
    maplist(length, Perms, Comprimentos),                 % comprimentos das permutacoes
    min(Comprimentos, Min),                               % menor dos comprimentos
    escolher_menor(Possiveis_aux, Escolha, Min).              

escolher_menor([[Espaco, Perms] | _], Escolha, Min) :-    % auxiliar que verifica se a permutacao 
    length(Perms, Min), !,                                % analisada tem o menor comprimento
    Escolha = [Espaco, Perms].

escolher_menor([_ | T], Escolha, Min) :-
    escolher_menor(T, Escolha, Min). 


/*
experimenta_perm(Escolha, Perms_Possiveis,Novas_Perms_Possiveis), 
em que Perms_Possiveis eh uma lista de permutacoes possiveis, 
Escolha eh um dos seus elementos (escolhido  pelo  predicado anterior), 
segue os seguintes passos:
1.  Sendo Esp e Lst_Perms o espaco e a lista de permutacoes de Escolha, respectivamente, escolhe uma  
    permutacao de Lst_Perms, Perm.
2.  Unifica Espc om Perm.
3.  Novas_Perms_Possiveis eh o resultado de substituir, em Perms_Possiveis, o elemento Escolha pelo 
    elemento[Esp, [Perm]].
*/
aux_perms(Escolha,[ H | T ], [ H | T_aux]) :-
    aux_perms(Escolha, T, T_aux). 

aux_perms(Escolha,[ H | T ], [E_act| T ]) :-
    H == Escolha, !,
    nth0(0, Escolha, E),
    E_act=[E, [E]].

experimenta_perm(Escolha, Perms_Possiveis, Novas_Perms_Possiveis) :-
    nth0(0, Escolha, E),
    nth0(1, Escolha, Perms),
    member(Perm, Perms),
    Perm = E,
    aux_perms(Escolha, Perms_Possiveis, Novas_Perms_Possiveis).



/*
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis),em que Perms_Possiveis eh uma lista de 
permutacoes possiveis, significa que Novas_Perms_Possiveis eh o resultado de aplicar o algoritmo  
descrito  na  Seccao 2.2 a Perms_Possiveis.
*/
resolve_aux(Perms_Possiveis, Novas_Perms_Possiveis) :-
    escolhe_menos_alternativas(Perms_Possiveis, Escolha), !,
    experimenta_perm(Escolha, Perms_Possiveis, Aux),
    simplifica(Aux, Aux2),
    resolve_aux(Aux2, Novas_Perms_Possiveis).

resolve_aux(Perms_Possiveis, Perms_Possiveis).


/*
resolve(Puz), em que Puz eh um puzzle, resolve esse puzzle, isto eh, apos a invocacao deste predicado 
a grelha de Puz tem todas as variaveis substituidas por numeros que respeitam as restricoes Puz.
*/
resolve(Puz) :-
    inicializa(Puz, Perms_Possiveis),
    resolve_aux(Perms_Possiveis, _).
   
