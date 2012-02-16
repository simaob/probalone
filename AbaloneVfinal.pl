
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%  ABALONE EM PROLOG - Jose Branco  &&  Simao Castro - 16/11/2005 %%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:-use_module(library(lists)).
:-use_module(library(sockets)).
:-use_module(library(random)).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%  REPRESENTACAO DO ESTADO  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Lista de apoio à impressao de tabuleiro
tabuleiro_visi(
        [[0,0,0,6,a,7,0,0,0],
         [0,0,6,b,7,0,0],
         [0,6,c,7,0],
         [6,d,7],
         [e],
         [7,f,6],
         [0,7,g,6,0],
         [0,0,7,h,6,0,0],
         [0,0,0,7,i,6,0,0,0]]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%  PREDICADOS CENTRAIS DO JOGO  %%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Estado inicial da matriz correspondente ao tabuleiro
estado_inicial(
        [              [1,1,1,1,1],
                      [1,1,1,1,1,1],
                     [8,8,1,1,1,8,8],
                    [8,8,8,8,8,8,8,8],
                   [8,8,8,8,8,8,8,8,8],
                    [8,8,8,8,8,8,8,8],
                     [8,8,2,2,2,8,8],
                      [2,2,2,2,2,2],
                       [2,2,2,2,2]]).


%Menu inicial. Execute-se este para correr o programa.
inicia:-
        abolish(jogador/2), abolish(estado/4),
        apresentacao,
        menu,
        tipo_jogo,
        estado_inicial(Tab),
        (Pontos1 is 0), (Pontos2 is 0),
        joga(1,1,Tab,Pontos1,Pontos2).


%Função if.
if(A,B,_):- A,!,B.
if(_,_,C):- C.



apresentacao:-
        write('Abalone em Prolog - 16/11/2005 - Simao Castro & Jose Castelo Branco'), nl, nl.
        

%Dá ao utilizador a opcão de Jogar ou de ver as regras do jogo e como jogar.
menu:-
           write('o que deseja fazer?'),nl,
           write('Jogar (pressione 1) Ver Regras e Ajuda (pressione 2) Sair(pressione 3)'),nl,
           repeat, get_code(Let2), Let2>=49, Let2=<51,
           numero(Let2) ,conv(Let2,Let), if(Let==2,((ajuda),(menu)),(if(Let==3,(abort),true))),!.


%Impressoes de ajuda
ajuda:-
             write('----------------------------REGRAS DO ABALONE------------------------------'),nl,
             write('- objectivo: empurrar as pecas do adversario ate as expulsar do tabuleiro.'),nl,
             write('- vence o jogador que primeiro expulsar do tabuleiro 6 pecas do adversario.'),nl,
             write('- pode mover uma a tres pecas em qualquer direccao, uma casa de cada vez.'),nl,
             write('- apenas se podem empurrar pecas se na situacao de maioria.'),nl,
             write('- o numero maximo de berlindes que pode empurrar e de tres.'),nl,
             write('- o numero maximo de berlindes que pode ser empurrado e de dois.'),nl,nl,nl,
             write('---------------------------COMO INSERIR JOGADAS----------------------------'),nl,
             write('- quando lhe pedir as  coordenadas iniciais, insira a posicao  da primeira ') ,nl,
             write('peca a empurrar. insira a coluna (letras) seguida  da linha (algarismos) ') ,nl,
             write('- quando lhe pedir  as  coordenadas finais, insira a  posicao da casa para ') ,nl,
             write('onde a primeira ira quando empurrar.                exemplo: |:A2 |:A3   ') ,nl,nl,nl.
             



%Função principal do programa
joga(N,J,Tab,Pontos1act,Pontos2act):-
        write('Jogada: '), write(N), write('   Jogador:'), write(J), nl,
        jogador(J,TipoJ),
        visualiza_estado(Tab, Pontos1act,Pontos2act),
        determina_jogada(J,TipoJ,Mov,Tab,(H,V),Xlast,Ylast),
        nl,
        executa_jogada(J,Mov,Xlast,Ylast,(H,V),Tab, NovoTab,Pontos1act,Pontos2act,Pontos1,Pontos2),
        nl,write('-------------------------------'),nl,
        guarda_estado(N,J,Mov,NovoTab),
        if(Pontos1==6, (visualiza_estado(NovoTab,Pontos1,Pontos2),fim_jogo(J)),
        (if(Pontos2 ==6,(visualiza_estado(NovoTab, Pontos1,Pontos2),fim_jogo(J)),continua(N,J,NovoTab,Pontos1,Pontos2)))).



%Menu onde se especifica que tipo de jogo queremos jogar
tipo_jogo:-
        write('1- Humano VS Computador, 2-Humano VS Humano, 3-Computador VS Computador'), nl,
        repeat, get_code(Let), Let>=49, Let=<51,
        tipoj(Let,Jog1,Jog2),
        assert(jogador(1,Jog1)), assert(jogador(2,Jog2)).


%Associacao do numero inserido à opção desejada
tipoj(49,humano,comp).
tipoj(50,humano,humano).
tipoj(51,comp,comp).


%Função de término do jogo
fim_jogo(Venc):-
        write('Fim do Jogo. Vencedor: Jogador '), write(Venc), nl,nl, inicia.


%Passa para a jogada seguinte. Incrementao numero da jogada e muda o jogador
continua(N,J,Tab,Pontos1,Pontos2):-
        N2 is N+1,
        troca(J,J2),
        joga(N2,J2,Tab,Pontos1,Pontos2).


%Guarda o estado modificado
guarda_estado(N,J,Mov,NovoTab):-
        assert(estado(N,J,Mov,NovoTab)).



%Recebe a jogada desejada de um utilizador
pede_jogada(X,Y):-
                  repeat, get_code(Sx), ( maiuscula(Sx);minuscula(Sx)), conv(Sx,X), X>0, X<10,
                  get_code(Sy),numero(Sy) ,conv(Sy,Y), Y>0, Y<10 ,!.
                  

%Converte os valores ASCII para reais
conv(Let,Valor):- maiuscula(Let), Valor is Let-64 .
conv(Let,Valor):- minuscula(Let), Valor is Let-96 .
conv(Let,Valor):- numero(Let), Valor is Let-48 .


%Indica se o valor ASCII é uma letra maiuscula, minuscula ou um numero
maiuscula(Let):- Let>=65, Let=<73 .
minuscula(Let):- Let>=97, Let=<105 .
numero(Let):- Let>=49, Let=<57 .



%Obtem uma jogada de um humano
determina_jogada(J,humano,Mov,Tab,(H,V), Xlast, Ylast):-
  repeat,
    nl, write('Escreva as coordenadas iniciais:'), nl,
    pede_jogada(X,Y),
    nl, write('Escreva as coordenadas finais:'), nl,
    pede_jogada(Xf,Yf),nl,
    converte_real(X,Y,Xf,Yf,XC,XfC),
    Mov =(XC,Y)-(XfC,Yf),
    movimento_valido(J,Mov,Tab,(H,V),Xlast,Ylast),!.


%Obtem uma jogada do computador
determina_jogada(J,comp,Mov,Tab,(H,V),Xlast,Ylast):-
        repeat,calcula_jogada(J,Mov,Tab,(H,V),Xlast,Ylast),!.
    


%Converte o valor que o utilizador indica no jogo para o verdadeiro indice nas listas
converte_real(X,Y,Xf,Yf,XC,XfC):-
              acerta_colunas(X,Y,XC),
              acerta_colunas(Xf,Yf,XfC),!.


%Acerta o valor de X
acerta_colunas(X,Y,Xfinal):- Y=<5,Xfinal is X.
acerta_colunas(X,Y,Xfinal):-
          X2 is X-1, Y2 is Y-1,
          acerta_colunas(X2,Y2,Xfinal).



%Executa uma jogada
executa_jogada(J,(X,Y)-(Xf,Yf),Xlast,Ylast,(H,V),Tab, NovoTab,Pontos1act,Pontos2act,Pontos1,Pontos2):-
                  ((\+dentro(Xlast,Ylast),soma_pontos(J,Pontos1act,Pontos2act,Pontos1,Pontos2));
                  (Pontos1 is Pontos1act),(Pontos2 is Pontos2act)),
                  oposto((H,V),(Hinv,Vinv)),!,
                  exec_move(J,X,Y,Xf,Yf,Xlast,Ylast,(Hinv,Vinv),Tab,NovoTab).


exec_move(J,X,Y,_,_,Xlast,Ylast,(Hinv,Vinv),Tab,NovoTab):-
                executa_movimento(J,X,Y,Xlast,Ylast,(Hinv,Vinv),Tab,NovoTab).


executa_movimento(_,X,Y,X,Y,(_,_),Tab,NovoTab):-
                 peca(P,X,Y,Tab),
                 muda_tab(P,8,X,Y,Tab,NovoTab).

                 
executa_movimento(J,X,Y,Xlast,Ylast,(Hinv,Vinv),Tab,NovoTab):-
                 \+dentro(Xlast,Ylast),(
                 direccao(Xlast,Ylast,Xseguinte,Yseguinte,(Hinv,Vinv)),
                 executa_movimento(J,X,Y,Xseguinte,Yseguinte,(Hinv,Vinv),Tab,NovoTab));
                 
                 (
                 direccao(Xlast,Ylast,Xseguinte,Yseguinte,(Hinv,Vinv)),
                 peca(P,Xlast,Ylast,Tab),
                 peca(P2,Xseguinte,Yseguinte,Tab),
                 muda_tab(P,P2,Xlast,Ylast,Tab,Tab2),!,
                 executa_movimento(J,X,Y,Xseguinte,Yseguinte,(Hinv,Vinv),Tab2,NovoTab)).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  FUNCOES AUXILIARES %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Verifica se o ponto X,Y está dentro do tabuleiro
dentro(X,1):-X>0,X<6 .
dentro(X,2):-X>0,X<7 .
dentro(X,3):-X>0,X<8 .
dentro(X,4):-X>0,X<9 .
dentro(X,5):-X>0,X<10 .
dentro(X,6):-X>0,X<9 .
dentro(X,7):-X>0,X<8 .
dentro(X,8):-X>0,X<7 .
dentro(X,9):-X>0,X<6 .


%Define a direccao oposta à recebida
oposto((c,d),(b,e)).
oposto((c,e),(b,d)).
oposto((b,e),(c,d)).
oposto((b,d),(c,e)).
oposto((l,e),(l,d)).
oposto((l,d),(l,e)).



%Com base no Y e na direccao calcula o incremento que é preciso dar a um ponto para passar à proxima casa nessa direccao
vector_direccao(Y,(c,d),IncX,IncY):-
              (Y=<5,(IncX is 0, IncY is -1)); (IncX is 1,IncY is -1).
vector_direccao(Y,(c,e),IncX,IncY):-
              (Y=<5,(IncX is -1, IncY is -1)); (IncX is 0, IncY is -1).
vector_direccao(Y,(b,d),IncX,IncY):-
              (Y<5, (IncX is 1, IncY is 1));(IncX is 0, IncY is 1).
vector_direccao(Y,(b,e),IncX,IncY):-
              (Y<5, (IncX is 0, IncY is 1));(IncX is -1, IncY is 1).
vector_direccao(_,(l,e),IncX, IncY):-
              IncX is -1, IncY is 0 .
vector_direccao(_,(l,d), IncX, IncY):-
              IncX is 1, IncY is 0 .
              

%Com base em dois pontos, obtem a direccao entre o inicial e final
direccao(X,Y,X2,Y2,(c,d)):- if(Y=<5,((X2 is X),(Y2 is Y-1),(dentro(X2,Y2))),((X2 is X+1),(Y2 is Y-1),(dentro(X2,Y2)))).
direccao(X,Y,X2,Y2,(c,e)):- if(Y=<5,((X2 is X-1),(Y2 is Y-1),(dentro(X2,Y2))),((X2 is X),(Y2 is Y-1),(dentro(X2,Y2)))).
direccao(X,Y,X2,Y2,(b,d)):- if(Y<5,((X2 is X+1),(Y2 is Y+1),(dentro(X2,Y2))),((X2 is X),(Y2 is Y+1),(dentro(X2,Y2)))).
direccao(X,Y,X2,Y2,(b,e)):- if(Y<5,((X2 is X),(Y2 is Y+1),(dentro(X2,Y2))),((X2 is X-1),(Y2 is Y+1),(dentro(X2,Y2)))).
direccao(X,Y,X2,Y2,(l,e)):- (X2 is X-1),(Y2 is Y),(dentro(X2,Y2)).
direccao(X,Y,X2,Y2,(l,d)):- (X2 is X+1),(Y2 is Y), (dentro(X2,Y2)).




%Verifica se a casa nas coordenadas X,Y está livre
livre(X,Y,Tab):- membrotab(8,X,Y,Tab).


%Verifica se na posicao X,Y do Tabuleiro está uma peca (representadas por 1 ou 2)
peca(J,X,Y,Tab):- membrotab(J,X,Y,Tab).




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%% Manipulacao do tabuleiro %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%Retorna o valor da peça na posição X,Y do tabuleiro
membrotab(Pec,X,Y,Tab):-
        membro_pos_lista(Linha, Y, Tab),
        membro_pos_lista(Pec, X, Linha).


%Procura um membro "M" numa lista
membro_pos_lista(Membro, N, Lista):-
        membro_pos_procura(Membro, 1, N, Lista).


membro_pos_procura(Membro, N, N, [Membro|_]).
membro_pos_procura(Membro, P, N, [_|T]):-
        P2 is P+1,
        membro_pos_procura(Membro, P2, N, T).




%Altera a informação de uma casa num tabuleiro
muda_tab(Peca,Pnov,X,Y,Tab,NovoTab):-
  muda_tab2(1,Peca,Pnov,X,Y,Tab,NovoTab),!.

muda_tab2(_,_,_,_,_,[],[]).
muda_tab2(Y,Peca,Pnov,X,Y,[Lin|Resto],[NovLin|Resto2]):-
                                                        muda_linha(1,Peca,Pnov,X,Lin,NovLin),
                                                        N2 is Y+1,
                                                        muda_tab2(N2,Peca,Pnov,X,Y,Resto,Resto2).

muda_tab2(N,Peca,Pnov,X,Y,[Lin|Resto],[Lin|Resto2]):-
                                                     N2 is N+1,
                                                     muda_tab2(N2,Peca,Pnov,X,Y,Resto,Resto2).
muda_linha(_,_,_,_,[],[]).
muda_linha(X,Peca,Pnov,X,[Peca|Resto],[Pnov|Resto2]):-
                                                      N2 is X+1,
                                                      muda_linha(N2,Peca,Pnov,X,Resto,Resto2).
muda_linha(N,Peca,Pnov,X,[El|Resto],[El|Resto2]):-
                                                  N2 is N+1,
                                                  muda_linha(N2,Peca,Pnov,X,Resto,Resto2).




%Serve para trocar o jogador 1 pelo 2 e vice-versa
troca(1,2).
troca(2,1).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Pontuacao %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Incrementa as variaveis pontos
soma_pontos(P,Pontos1act,Pontos2act,Pontos1,Pontos2):-
            ((P==1,Pontos1 is Pontos1act+1, Pontos2 is Pontos2act);
            (P==2,Pontos2 is Pontos2act+1, Pontos1 is Pontos1act)).


%Imprime a pontuação
pontuacao(Pontos1,Pontos2):-
           nl, write('B = '), write(Pontos1), write(' '),
           pecas_fora(Pontos1 , 2), nl,
           nl, write('P = '), write(Pontos2), write(' '),
           pecas_fora(Pontos2,1),nl.


%Imprime as peças fora
pecas_fora(0,_).
pecas_fora(Pontos, Peca):-
          escreve(Peca,_),
          Pontos2 is Pontos-1,
          pecas_fora(Pontos2,Peca).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%% Validação de movimentos %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%Verifica se um determinado movimento é valido
movimento_valido(P,(X,Y)-(Xf,Yf),Tab,(H,V), Xlast,Ylast):-
        peca(P2,X,Y,Tab), (P2 == P),
        direccao(X,Y,Xf,Yf,(H,V)),
        conta_pecas(P2,Xf,Yf,Tab,H,V,Xlast,Ylast,Same,Other),
        Same=<2,
        (Same+1)>Other,
        (Other\=0; dentro(Xlast,Ylast)).


%Conta todas as peças numa determinada direccao
conta_pecas(P2,Xf,Yf,Tab,H,V,Xlast,Ylast,Same,Other):-
                  conta_aux(P2,Xf,Yf,Tab,H,V,Xlast,Ylast,Same,Other,0,0,0).


conta_aux(P2,Xf,Yf,Tab,H,V,Xlast,Ylast,Same,Other,S2,O2,Out):-
                  ( (\+dentro(Xf,Yf);livre(Xf,Yf,Tab);Out>0),
                  (Same is S2,
                  Other is O2,
                  Xlast is Xf,!,
                  Ylast is Yf));
                  (
                  vector_direccao(Yf,(H,V),IncX,IncY),
                  peca(P,Xf,Yf,Tab),
                  Out2 is Out,
                  if((P2 ==P,O2==0),(S3 is S2+1, O3 is O2),
                  if((P2 ==P,O2>0), Out2 is Out+1,(O3 is O2+1, S3 is S2))),
                  Xf2 is Xf+IncX,
                  Yf2 is Yf+IncY,!,
                  conta_aux(P2,Xf2,Yf2,Tab,H,V,Xlast,Ylast,Same,Other,S3,O3,Out2)).
                  



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%% Inteligência Artificialmente Facciosa %%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%Calcula a jogada do computador
calcula_jogada(J,Mov,Tab,(H,V),Xlast,Ylast):-

                  %Empurrar Peça para fora andando maximo de casas
                 (findall(Mov-(H,V)-Xlast-Ylast,
                 (movimento_valido(J,Mov,Tab,(H,V),Xlast,Ylast),
                 \+dentro(Xlast,Ylast),distancia_maxima(Mov,Tab,(H,V),Xlast,Ylast,4)),Lista1),
                 Lista1 \= [],
                 length(Lista1,Comp),
                 Upper is Comp+1,
                 random(1,Upper,N),
                 membro_pos_lista(Mov-(H,V)-Xlast-Ylast,N,Lista1));
                 (
                 %Empurrar Peça para fora
                 findall(Mov-(H,V)-Xlast-Ylast,
                 (movimento_valido(J,Mov,Tab,(H,V),Xlast,Ylast),\+dentro(Xlast,Ylast)),Lista2),
                 Lista2 \= [],
                 length(Lista2,Comp),
                 Upper is Comp+1,
                 random(1,Upper,N),
                 membro_pos_lista(Mov-(H,V)-Xlast-Ylast,N,Lista2));
                 
                 (
                 pesos_destino(Tab,Xlast,Ylast,4),
                 findall(Mov-(H,V)-Xlast-Ylast,
                 (movimento_valido(J,Mov,Tab,(H,V),Xlast,Ylast),
                 distancia_maxima(Mov,Tab,(H,V),Xlast,Ylast,4)),Lista5),
                 Lista5 \= [],
                 length(Lista5,Comp),
                 Upper is Comp+1,
                 random(1,Upper,N),
                 membro_pos_lista(Mov-(H,V)-Xlast-Ylast,N,Lista5));

                 (
                 pesos_destino(Tab,Xlast,Ylast,4),
                 findall(Mov-(H,V)-Xlast-Ylast,
                 (movimento_valido(J,Mov,Tab,(H,V),Xlast,Ylast),
                 distancia_maxima(Mov,Tab,(H,V),Xlast,Ylast,3)),Lista3),
                 Lista3 \= [],
                 length(Lista3,Comp),
                 Upper is Comp+1,
                 random(1,Upper,N),
                 membro_pos_lista(Mov-(H,V)-Xlast-Ylast,N,Lista3));

                 %andar qualquer numero de casas
                 (
                 lista_jogadas(J,Tab,Lista),
                 Lista \= [],
                 length(Lista,Comp),
                 Upper is Comp+1,
                 random(1,Upper,N),
                 membro_pos_lista(Mov-(H,V)-Xlast-Ylast,N,Lista)).




%Calcula a distancia maxima de determinada jogada
distancia_maxima((X,Y)-(_,_),Tab,(H,V),Xlast,_,DisM):-
                     distancia_maxima((X,Y)-(_,_),Tab,(H,V),Xlast,_,DisM,0).
distancia_maxima((Xlast,_)-(_,_),_,(_,_),Xlast,_,DisM,DisM2):-
                         !,
                         DisM is DisM2.
distancia_maxima((X,Y)-(_,_),Tab,(H,V),Xlast,_,DisM,DisM2):-
                         vector_direccao(Y,(H,V),IncX,_),
                         X2 is X+IncX,
                         DisM3 is DisM2+1,!,
                         distancia_maxima((X2,Y)-(_,_),Tab,(H,V),Xlast,_,DisM,DisM3).

%Carrega para uma lista todos os movimentos possiveis na jogada actual
lista_jogadas(J, Tab, Lista):-
        findall(Mov-(H,V)-Xlast-Ylast, movimento_valido(J,Mov,Tab,(H,V),Xlast,Ylast), Lista).


%Quanto vale uma peça no final do movimento
pesos_destino(Tab,Xlast,Ylast,Peso):-
                    findall((X-Y), peca(8,X,Y,Tab),Lista),
                    pesos_casas(Pesos),
                    findall((X-Y),(peca(P,X,Y,Pesos),member((X-Y),Lista),P>Peso),ListaPesos),
                    ListaPesos \=[],
                    length(ListaPesos,Comp),
                    Upper is Comp+1,
                    random(1,Upper,N),
                    membro_pos_lista(Xlast-Ylast,N,ListaPesos).


%Prioridade para o computador de determinadas casas.
pesos_casas(
        [              [1,1,1,1,1],
                      [1,2,2,2,2,1],
                     [1,1,3,3,3,1,1],
                    [1,1,3,4,4,3,1,1],
                   [1,1,3,4,5,4,3,1,1],
                    [1,1,3,4,4,3,1,1],
                     [1,3,2,2,2,2,1],
                      [1,2,2,2,2,1],
                       [1,1,1,1,1]]).
                         
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%  VISUALIZACAO DO ESTADO DO JOGO - MODO DE TEXTO %%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%Visualiza o tabuleiro inteiro
visualiza_estado(Tab, Pontos1,Pontos2):-
        pontuacao(Pontos1,Pontos2),nl,
        nl, write('       A B C D E F G H I  '),
        nl, write('      _________     '), nl,
        tabuleiro_visi(Tab2),
        mostra_linhas(1,Tab2,Tab),
        write('      ¯¯¯¯¯¯¯¯¯     '), nl,!.


%Imprime as linhas do tabuleiro
mostra_linhas(_,[], _).
mostra_linhas(N,[Lin|Resto], Tab):-
        write(N), write(' '), mostra_linha(Lin,Tab), write(' '), nl,
        N2 is N+1,
        mostra_linhas(N2, Resto, Tab).

mostra_linha([], _).
mostra_linha([El|Resto], Tab):-
        escreve(El, Tab),
        mostra_linha(Resto, Tab).



%Imprime o estado actual do tabuleiro
mostra_estado(N,N,[Lin|_],NPecas):- mostra_estado_linha(Lin,NPecas).
mostra_estado(N,M,[_|Resto],NPecas):-
              M2 is M+1,
              mostra_estado(N,M2,Resto,NPecas).


%Imprime as linhas do estado actual do tabuleiro
mostra_estado_linha([],_).
mostra_estado_linha([E1|Resto],NPecas):-
        escreve(E1, _),
        NPecas2 is NPecas-1,
        if(NPecas2\=0, ((write('.')),(mostra_estado_linha(Resto,NPecas2))),mostra_estado_linha(Resto,NPecas2)).



%Traduz os caracteres nas listas de estado e impressao para o que queremos impresso no ecran
escreve(0, _):-write(' ').
escreve(1, _):-write('o').
escreve(2, _):-write('@').
escreve(6, _):-write('/').
escreve(7, _):-write('\\').
escreve(8, _):-write('x').
escreve(9, _):-write('.').
escreve(a,Tab):-mostra_estado(1,1,Tab,5).
escreve(b,Tab):-mostra_estado(2,1,Tab,6).
escreve(c,Tab):-mostra_estado(3,1,Tab,7).
escreve(d,Tab):-mostra_estado(4,1,Tab,8).
escreve(e,Tab):-mostra_estado(5,1,Tab,9).
escreve(f,Tab):-mostra_estado(6,1,Tab,8).
escreve(g,Tab):-mostra_estado(7,1,Tab,7).
escreve(h,Tab):-mostra_estado(8,1,Tab,6).
escreve(i,Tab):-mostra_estado(9,1,Tab,5).










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%  Sockets - Interface comunicacao %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


port(60001).

server:-
        current_host(Host),
        port(Port),
        socket('AF_INET', Socket),
        socket_bind(Socket, 'AF_INET'(Host,Port)),
        socket_listen(Socket, 5),
        socket_accept(Socket, _Client, Stream),
        server_loop(Stream),
        socket_close(Socket),
        write('Server Exit'),nl.

server_loop(Stream) :-
        repeat,
                read(Stream, ClientRequest),
                write('Received: '), write(ClientRequest), nl,
                server_input(ClientRequest, ServerReply),
                format(Stream, '~q.~n', [ServerReply]),
                write('Send: '), write(ServerReply), nl,
                flush_output(Stream),
        (ClientRequest == bye; ClientRequest == end_of_file), !.

server_input(initialize, ok(Tab)):-
        estado_inicial(Tab), !.
server_input(execute(Mov, Tab), ok(NovoTab, Pontos1, Pontos2)):-
        movimento_valido(1, Mov, Tab,(H,V),Xlast,Ylast),
        executa_jogada(1,Mov,Xlast,Ylast,(H,V),Tab, NovoTab,0,0,Pontos1,Pontos2), !.
server_input(calculate(J, Tab), ok(Mov, NovoTab, Pontos1, Pontos2)):-
        calcula_jogada(J,Mov,Tab,(H,V),Xlast,Ylast),
        executa_jogada(J,Mov,Xlast,Ylast,(H,V),Tab, NovoTab,0,0,Pontos1,Pontos2), !.
server_input(game_end(Pontos1,Pontos2), ok(Winner)):-
        if(Pontos1>=Pontos2,Winner is Pontos1, Winner is Pontos2), !.
server_input(bye, ok):-!.
server_input(end_of_file, ok):-!.
server_input(_, invalid) :- !.


