% lp24 - Inês de Oliveira Martins - ist1107083 - projecto 
:- use_module(library(clpfd)). % para poder usar transpose/2
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % ver listas completas
:- [puzzles]. % Ficheiro dado. A avaliação terá mais puzzles.
:- [codigoAuxiliar]. % Ficheiro dado. Não alterar.
% Atenção: nao deves copiar nunca os puzzles para o teu ficheiro de código
% Nao remover nem modificar as linhas anteriores. Obrigado.
% Segue-se o código
%%%%%%%%%%%%

/* --------------------------*
|  Operações de visualização |
*---------------------------*/

/**
* visualiza/1 - Imprime cada elemento de uma lista.
* @param Lista: Uma lista de elementos a serem impressos.
*               Caso a lista esteja vazia, nada será impresso.
*/
visualiza([]).

visualiza([H|T]) :- 
    write(H), nl,
    visualiza(T).

/**
* visualizaLinha/1 - Imprime cada elemento de uma lista e respetivo índice, por linha.
* @param Lista: Uma lista de elementos a serem impressos.
*               Caso a lista esteja vazia, nada será impresso.
*/

% Acumulador é adicionado para ser usado como índice 
visualizaLinha(L) :- visualizaLinha(L,1). 

visualizaLinha([], _).

visualizaLinha([H|T], Ac):-
    write(Ac),write(": "),
    visualiza([H]),
    Ac1 is Ac+1,
    visualizaLinha(T, Ac1).

/**
* coordVal/3 - Verifica se uma dada coordenada (L,C) pertence a uma matriz T.
* @param L: Linha em que se encontra a coordenada.
* @param C: Coluna em que se encontra a coordenada.
* @param T: Matriz a verificar se coordenada está contida.
*/
coordVal(L,C,T) :-
    length(T, NumLinhas),
    L > 0, L =< NumLinhas,
    nth1(L, T, Linha),
    length(Linha, NumColunas),
    C > 0, C =< NumColunas. 


/**
* insereObjecto/3 - Insere dado numa coordenada específica duma matriz.
* @param (L, C): Coordenada.
*                Se esta não for válida ou já estiver ocupada, matriz permanece igual
* @param T: Matriz onde será inserido o novo dado.
* @param Obj: Dado a ser inserido.
*/
insereObjecto(_, [], _) :- !.

insereObjecto((L, C), T, Obj) :-
    coordVal(L, C, T),  % confirma que coordenada é válida
    nth1(L, T, Linha),    
    nth1(C, Linha, Cell), % localiza célula a ser alterada
    var(Cell),
    Cell=Obj, !. 

% coordenada inválida, matriz fica igual
insereObjecto((L, C), Tab, _) :-
    \+ coordVal(L, C, Tab), 
    Tab = Tab, !.

% célula já tem um valor, matriz permanece igual
insereObjecto((L, C), T, _) :-
    coordVal(L, C, T),
    nth1(L, T, Linha),  
    nth1(C, Linha, Cell), 
    (Cell == e; Cell == p), 
    !.

/**
* insereVariosObjetos/3 - insere dados em células específicas duma matriz
* @param ListaCoords: Lista de Coordenadas
* @param T: Matriz onde serão inseridos os novos dados.
* @param ListaObjs: Lista de dados a serem inserido.
*
* Predicado falha se ListaCoords e ListaObjs tiverem comprimentos diferentes.
*/
insereVariosObjectos([], _, []):- !.

insereVariosObjectos([C|L], T, [O|L2]) :-
    % chama predicado insereObjeto para inserir elemento O em C
    ( insereObjecto(C, T, O) -> true ; true ), 
    % recursão para resto das listas
    insereVariosObjectos(L, T, L2), !.

insereVariosObjectos(ListaCoords, _, ListaObjs) :-
    length(ListaCoords, N1),
    length(ListaObjs, N2),
    N1 \= N2, !, fail. 

/**
* inserePontosVolta/2 - insere pontos à volta da coordenada dada
* @param T: Matriz onde serão inseridos os pontos.
* @param (L,C): Coordenada.
*/
inserePontosVolta([], _):- !.

inserePontosVolta(T, (L, C)) :-
    % Definir coordenadas à volta da coordenada dada
    L1 is L-1, L2 is L+1, 
    C1 is C-1, C2 is C+1,
    % recurso a insereVariosObjetos para preencher coordenadas anteriores
    insereVariosObjectos([(L1, C1), (L1, C), (L1, C2),
                          (L, C1), (L, C2),
                          (L2, C1), (L2, C), (L2, C2)],
                         T, [p, p, p, p, p, p, p, p]), !.

/**
* inserePontos/3 - insere pontos à volta de coordenadas dadas
* @param T: Matriz onde serão inseridos os pontos.
* @param ListaCoords: Lista de Coordenadas
*/
inserePontos(_, []):- !.

inserePontos(T, [(L,C)|L1]) :-
    insereObjecto((L, C), T, p),
    inserePontos(T, L1).


/* ---------------------*
| Operações de consulta |
*----------------------*/

/**
* objectosEmCoordenadas/3 - Obtém a lista de objetos que se encontram nas 
                            coordenadas especificadas dum tabuleiro.
* @param ListaCoords: Lista de coordenadas
* @param Tabuleiro: Matriz onde pesquisa é realizada.
* @param ListaObjs: A lista de objetos correspondentes às coordenadas fornecidas em ListaCoords.
*
* O predicado falha se alguma coordenada não pertencer ao tabuleiro.
*/
objectosEmCoordenadas([], _,[]).

objectosEmCoordenadas([(L,C)|Coords], T, [O|Objs]) :-
    % Verifica validade da coordenada
    coordVal(L,C, T), !,

    % Encontra objeto presente na coordenada
    nth1(L, T, Linha),    
    nth1(C, Linha, Obj),

    % Coloca objeto na lista de objetos
    Obj = O,
    objectosEmCoordenadas(Coords, T, Objs).

/**
* coordObjectos/5 - Filtra as coordenadas de um tipo de objeto e conta sua ocorrência no tabuleiro.
* @param Objecto (O): O tipo de objeto a ser procurado.
* @param Tabuleiro (T): Matriz onde a pesquisa é realizada.
* @param ListaCoords: A lista de todas as coordenadas do tabuleiro.
* @param ListaCoordObjs (LCO): Sublista de ListaCoords que contem apenas 
                               as coordenadas dos objetos do tipo do Objecto.
* @param Num: O número total de objetos do tipo Objecto encontrados nas coordenadas.
*/
coordObjectos(_, _, [], [], 0):- !.

coordObjectos(O, T, [(L, C)|Rest], LCO, Num) :- 
    % Ver se Objecto é uma variável
    var(O), !,

    % Encontra elemento na coordenada
    nth1(L, T, Linha),
    nth1(C, Linha, El),

    % recursão por restantes valores 
    coordObjectos(O, T, Rest, RestLCO, RestNum),

    % Se elemento é uma var então adiciona-se a LCO
    (var(El) -> 
        LCO = [(L, C)|RestLCO],
        Num is RestNum + 1
    ; 
        LCO = RestLCO,
        Num = RestNum
    ).

coordObjectos(O, T, [(L, C)|Rest], LCO, Num) :- 
    % Neste caso, O não é uma var
    nth1(L, T, Linha),
    nth1(C, Linha, El),
    coordObjectos(O, T, Rest, UnsortedLCO, RestNum),

    % Se elemento na coordenada corresponder com objeto, adiciona-se a LCO
    (O == El -> 
        LCO = [(L, C)|UnsortedLCO],
        Num is RestNum + 1
    ; 
        LCO = UnsortedLCO,
        Num = RestNum
    ).

/**
* coordenadasVars/2 - Obtém as coordenadas das variáveis num tabuleiro.
* @param Tabuleiro: Matriz onde as variáveis são procuradas.
* @param ListaVars: A lista ordenada de coordenadas onde se encontram variáveis no tabuleiro.
*/
coordenadasVars(Tabuleiro, ListaVars) :- 
    % Encontra todas as coordenadas onde está uma variável
    findall((L,C), 
            (nth1(L, Tabuleiro, Linha),
             nth1(C, Linha, El),
             var(El)),
            TempVars),
    sort(TempVars, ListaVars).

/* -----------------------*
| Operações de estratégia |
*------------------------*/

/**
* fechaListaCoordenadas/2 - Atualiza tabuleiro com base em coordenadas, 
                            alterando-as para estrelas e pontos.
* @param Tabuleiro: O tabuleiro original, que pode ter diferentes tipos de objetos nas coordenadas.
* @param ListaCoord: A lista de coordenadas que serão verificadas e, se necessário, atualizadas.
*/
fechaListaCoordenadas(_, []):- !.

fechaListaCoordenadas(Tabuleiro, ListaCoord) :-

    % Verificar nº de estrelas e de variáveis livres
    coordObjectos(e, Tabuleiro, ListaCoord, _, NumEstrelas),
    coordObjectos(_, Tabuleiro, ListaCoord, VariaveisLivres, NumLivres),

    (NumEstrelas == 2 -> 
        inserePontos(Tabuleiro, VariaveisLivres), !

    ; (NumEstrelas == 1, NumLivres == 1 ->
        % Coord passa a ser coordenada da variável livre
        [Coord] = VariaveisLivres,
        % inserir estrela em Coord e colocar pontos à volta
        insereObjecto(Coord, Tabuleiro, e),
        inserePontosVolta(Tabuleiro, Coord), !

    ; (NumEstrelas == 0, NumLivres == 2 ->
        [(L1,C1), (L2,C2)] = VariaveisLivres,
        % inserir estrelas nas variaveis livres e pontos à volta das mesmas
        insereObjecto((L1,C1), Tabuleiro, e),
        insereObjecto((L2,C2), Tabuleiro, e),
        inserePontosVolta(Tabuleiro, (L1,C1)),
        inserePontosVolta(Tabuleiro, (L2,C2)), !
    )); true).


/**
* fecha/2 - Atualiza tabuleiro consoante listas de listas de coordenadas
* @param Tabuleiro: Matriz original, será alterada com base nas coordenadas de cada lista.
* @param ListaListasCoord: Uma lista de listas de coordenadas.
                           Cada sublista será processada separadamente.
*/
fecha(_, []) :- !.

fecha(Tabuleiro, [List| Resto]) :-
    fechaListaCoordenadas(Tabuleiro, List), !,
    fecha(Tabuleiro, Resto).

/* ------------------*
|  Encontrar Padrões |
*-------------------*/

/**
* mesmaLinha/2 - Predicado auxiliar que verifica se coordenadas estão na mesma linha.
* @param Tabuleiro: Matriz onde as coordenadas serão verificadas.
* @param Coord: A lista de coordenadas a serem verificadas.
*/
mesmaLinha(_, []):-!.
mesmaLinha(Tabuleiro, [(L,C)|Coord]) :-
    coordVal(L,C,Tabuleiro),
    mesmaLinhaHelper(Tabuleiro, L, Coord).

% verifica se coordenada é válida e se o valor L é sempre o mesmo    
mesmaLinhaHelper(_,_, []) :-!.
mesmaLinhaHelper(Tabuleiro, L, [(L,C)|Coord]) :-
    coordVal(L,C,Tabuleiro),
    mesmaLinhaHelper(Tabuleiro, L, Coord).

/**
* mesmaColuna/2 - Predicado auxiliar que verifica se coordenadas estão na mesma coluna.
* @param Tabuleiro: Matriz onde as coordenadas serão verificadas.
* @param Coord: A lista de coordenadas a serem verificadas.
*/
mesmaColuna(_, []):-!.
mesmaColuna(Tabuleiro, [(L,C)|Coord]) :-
    coordVal(L,C,Tabuleiro),
    mesmaColunaHelper(Tabuleiro, C, Coord).

% verifica se coordenada é válida e se o valor C é sempre o mesmo 
mesmaColunaHelper(_,_, []):-!.
mesmaColunaHelper(Tabuleiro, C, [(L,C)|Coord]) :-
    coordVal(L,C,Tabuleiro),
    mesmaColunaHelper(Tabuleiro, C, Coord).

/**
* mesmaRegiao/3 - Predicado auxiliar que verifica se as coordenadas estão na mesma região num tabuleiro.
* @param T: O tabuleiro onde as coordenadas serão verificadas.
* @param Regioes: A lista de regiões possíveis no tabuleiro.
* @param Coordenadas: A lista de coordenadas a serem verificadas.
*/
mesmaRegiao(_, [], _):- !.
mesmaRegiao(T, [Regiao|RestRegioes], Coordenadas) :-
    (mesmaRegiaoHelper(T, Regiao, Coordenadas);
    mesmaRegiao(T, RestRegioes, Coordenadas)).

% Verifica se coordenadas são válidas e se pertencem a Regiao
mesmaRegiaoHelper(_, _, []):- !. 
mesmaRegiaoHelper(T, Regiao, [(L,C)|CoordRestante]) :-
    % Verifica se coordenadas são válidas e se pertencem à regiao
    coordVal(L,C,T),
    member((L,C), Regiao),   
    mesmaRegiaoHelper(T, Regiao, CoordRestante).

/**
* coordenadasSeguidas/3 - Verifica se as coordenadas estão na mesma linha, coluna ou região.
* @param T:Matriz ou tabuleiro onde as coordenadas serão verificadas.
* @param Regioes: Lista de regiões do tabuleiro.
* @param Coordenadas: Lista de coordenadas a serem verificadas.
*/
coordenadasSeguidas(_, _, []) :- !.
coordenadasSeguidas(T, Regioes, Coordenadas) :-
    (   mesmaLinha(T, Coordenadas), !  
    ;   mesmaColuna(T, Coordenadas), !        
    ;   mesmaRegiao(T, Regioes, Coordenadas), ! 
    ).

coordenadasSeguidas(_,_,[]) :- !.
coordenadasSeguidas(T, Regioes, Coordenadas) :-
    (   mesmaLinha(T, Coordenadas), !  
    ;   mesmaColuna(T, Coordenadas), !        
    ;   mesmaRegiao(T, Regioes, Coordenadas), ! 
    ).

/**
* coordenadasSeguidas/2 - Verifica se as coordenadas estão na mesma linha ou coluna.
* @param T: O tabuleiro onde as coordenadas serão verificadas.
* @param Coordenadas: A lista de coordenadas a serem verificadas.
*/
coordenadasSeguidas(_,[]):- !.
coordenadasSeguidas(T, Coordenadas) :-
    (   mesmaLinha(T, Coordenadas), !         
    ;   mesmaColuna(T, Coordenadas), ! 
    ).

/**
* encontraSequencia/4 - Encontra uma sequência de coordenadas com variáveis 
                        seguidas em uma linha, coluna ou região.
* @param Tabuleiro: Tabuleiro onde as coordenadas se encontram.
* @param N: O tamanho da sequência que deve ser encontrada.
* @param ListaCoords: A lista de coordenadas a ser considerada.
* @param Seq: A sequência de coordenadas encontrada, 
              que é uma sublista de ListaCoords com N coordenadas.
*/
encontraSequencia(Tab, N, ListaCoords, Seq) :-
    % Feita cópia de Tabuleiro para evitar erros
    copy_term(Tab, TabCopy),

    % ListaCoords não pode conter estrelas
    coordObjectos(e, TabCopy, ListaCoords, _, NumE),
    NumE == 0, 

    % Coordenadas representam posições com variáveis ou pontos
    coordsComVarsOuPontos(TabCopy, ListaCoords),

    % Coordenadas aparecem seguidas
    (coordRegioes(TabCopy, Regioes) -> 
        coordenadasSeguidas(TabCopy, Regioes, ListaCoords)
    ;   
        coordenadasSeguidas(ListaCoords)
    ),

    % Concatenação de Seq com prefixo e sufixo
    append(Preffix, SeqComSuffix, ListaCoords),
    append(Seq, Suffix, SeqComSuffix),
    length(Seq, N),

    % Prefixo e sufixo podem ser vazios ou conter apenas pontos
    (Preffix == [] ; apenasPontos(TabCopy, Preffix) ; apenasVars(TabCopy, Preffix)),
    (Suffix == [] ; apenasPontos(TabCopy, Suffix) ; apenasVars(TabCopy, Suffix)),

    % Seq deve ser diferente do prefixo e do sufixo
    (apenasPontos(TabCopy, Seq) ->  apenasVars(TabCopy, Preffix), apenasVars(TabCopy, Suffix);
     apenasVars(TabCopy, Seq) -> apenasPontos(TabCopy, Preffix), apenasPontos(TabCopy, Suffix)),

    % Seq deve conter apenas variáveis ou apenas pontos
    (coordsComVars(TabCopy, Seq) ; coordsComPontos(TabCopy, Seq)).

/**
* apenasPontos/2 - Predicado auxiliar que verifica se todas as coordenadas na lista contêm pontos.
* @param Tab: Tabuleiro onde as coordenadas serão verificadas.
* @param Coords: A lista de coordenadas a serem verificadas.
*/
apenasPontos(_, []) :- !.
apenasPontos(Tab, [(L,C)|Rest]) :-
    nth1(L, Tab, Linha),
    nth1(C, Linha, Elem),
    Elem == p, !,
    apenasPontos(Tab, Rest).

/**
* apenasVars/2 - Predicado auxiliar que verifica se todas as coordenadas numa lista são variáveis livres.
* @param Tab: O tabuleiro onde as coordenadas serão verificadas.
* @param Coords: A lista de coordenadas a serem verificadas.
*/
apenasVars(_, []) :- !.
apenasVars(Tab, [(L,C)|Rest]) :-
    nth1(L, Tab, Linha),
    nth1(C, Linha, Elem),
    var(Elem), !,
    apenasVars(Tab, Rest).

/**
* coordsComVars/2 - Verifica se todas as coordenadas contêm apenas variáveis.
* @param Tab: O tabuleiro cujas coordenadas serão verificadas.
* @param Coords: A lista de coordenadas a serem verificadas.
*/
coordsComVars(_, []):- !.
coordsComVars(Tab, [(L,C)|Rest]) :-
    nth1(L, Tab, Linha),
    nth1(C, Linha, El),
    (var(El)), !,
    coordsComVars(Tab, Rest).

/**
* coordsComPontos/2 - Verifica se todas as coordenadas contêm apenas pontos.
* @param Tab: Tabuleiro cujas coordenadas serão verificadas.
* @param Coords: A lista de coordenadas a serem verificadas.
*/
coordsComPontos(_, []):- !.
coordsComPontos(Tab, [(L,C)|Rest]) :-
    nth1(L, Tab, Linha),
    nth1(C, Linha, El),
    (El==p),
    coordsComPontos(Tab, Rest).

/**
* coordsComVarsOuPontos/2 - Verifica se as coordenadas contêm apenas variáveis ou pontos.
* @param Tab: O tabuleiro onde as coordenadas serão verificadas.
* @param Coords: A lista de coordenadas a serem verificadas.
*/
coordsComVarsOuPontos(_, []):- !.
coordsComVarsOuPontos(Tab, [(L,C)|Rest]) :-
    nth1(L, Tab, Linha),
    nth1(C, Linha, El),
    (var(El); El==p), !,
    coordsComVarsOuPontos(Tab, Rest).

/**
* verificaTabuleiro/1 - Verifica se o tabuleiro é uma lista válida de listas.
* @param Tabuleiro: O tabuleiro a ser verificado.
*/
verificaTabuleiro(Tabuleiro) :-
    is_list(Tabuleiro), !,
    maplist(verificaLinha, Tabuleiro).

/**
* verificaLinha/1 - Verifica se uma linha é uma lista válida de elementos.
* @param Linha: A linha a ser verificada.
*/
verificaLinha(Linha) :-
    is_list(Linha), !,
    maplist(verificaElemento, Linha).

/**
* verificaElemento/1 - Verifica se um elemento é válido (var, p ou e).
* @param Elemento: Elemento a ser verificado.
*/
verificaElemento(Elemento) :-
    var(Elemento), !; Elemento == e, !; Elemento == p, !.

/**
* verificaCoordenadas/1 - Verifica se as coordenadas são válidas.
* @param Coords: Lista de coordenadas a ser verificada.
*/
verificaCoordenadas([]). 
verificaCoordenadas([(L, C) | Rest]) :-
    % Verifica se L e C são inteiros
    integer(L),        
    integer(C),          
    verificaCoordenadas(Rest). 

/**
* aplicaPadraoI/2 - Aplica padrão com base nas coordenadas fornecidas.
* @param Tabuleiro: Tabuleiro onde o padrão será aplicado.
* @param Coordenadas: Lista de 3 coordenadas da forma (L,C).
*/
aplicaPadraoI(Tabuleiro, [(L1, C1), (L2,C2), (L3, C3)]) :-
    verificaTabuleiro(Tabuleiro),
    verificaCoordenadas([(L1, C1), (L2,C2), (L3, C3)]),
    insereObjecto((L1,C1), Tabuleiro, e), !,
    insereObjecto((L3,C3), Tabuleiro, e), !,
    inserePontosVolta(Tabuleiro, (L1,C1)), !,
    inserePontosVolta(Tabuleiro, (L3,C3)).

/**
* aplicaPadroes/2 - Aplica padrões com base nas listas de coordenadas fornecidas.
* @param Tab: O tabuleiro onde os padrões serão aplicados.
* @param ListCoords: Lista de listas de coordenadas.
*/
aplicaPadroes(_, []).

aplicaPadroes(Tab, ListCoords) :-
    member(Coord, ListCoords),
    (encontraSequencia(Tab, 3, Coord, Seq) -> (forall(Seq, aplicaPadraoI(Tab, Seq))); true),
    (encontraSequencia(Tab, 4, Coord, Seq) -> (forall(Seq, aplicaPadraoT(Tab, Seq))); true).

/* ------------------*
|   Apoteose Final   |
*-------------------*/

/**
* resolve/2 - Resolve o desafio aplicando padrões e fechando o tabuleiro até não haver mais alterações.
* @param Estruturas: Estruturas que caraterizam o tabuleiro.
* @param Tab: O tabuleiro a ser modificado até alcançar uma solução.
*/
resolve([], _).
resolve(Estruturas, Tab) :-
    % Arranjar todas as coordenadas
    coordTodas(Estruturas, ListCoord),

    % coordenadas das variáveis livres antes de aplicar padrões
    coordenadasVars(Tab, List1),
    aplicaPadroes(Tab, ListCoord),
    fecha(Tab, ListCoord),

    % coordenadas das variáveis livres depois de aplicar padrões
    coordenadasVars(Tab, List2),

    % Continuar se número de variaveis livres diminuiu
    (List1 \= List2 -> resolve(Estruturas, Tab); true).