      *===============================================================*
      * INC          : I#DCOMV6                                       *
      * DESCRICAO    : LAYOUT DA MENSAGEM COR0005R1                   *
      * DATA CRIACAO : JUNHO/2017                                     *
      * AUTOR        : BRQ SOLUCOES EM INFORMATICA S.A.               *
      * TAMANHO      : 065                                            *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *================================================================*
      *                   A L T E R A C A O                            *
      *----------------------------------------------------------------*
V2506 *  ANALISTA......:  VAGNER MANFRINATO    -   BRADESCO
      *  DATA..........:  JUNHO/2025                                   *
      *----------------------------------------------------------------*
      *  OBJETIVO......:   ADEQUACAO SICOR 5.10                        *
      *                    ALTERACOES NA COR0005R1:                    *
      *                    - INCLUIDOS NA MENSAGEM O GRUPO ABAIXO, QUE *
      *                      NAO EH OBRIGATORIO E SERA TRATADO INTERNA-*
      *                      MENTE NO PROGRAMA, ASSIM COMO AS DATAS    *
      *                      QUANDO TIVEREM REJEICOES, PORQUE A PREVI- *
      *                      SAO EH QUE VENHAM NA SEQUENCIA (APOS A UL-*
      *                      TIMA OCORRENCIA).                         *
      *                        - GRUPO REJEICAO OPERACAO COR           *
      *                        - CODIGO REJEICAO OPERACAO COR          *
      *                        - TEXTO REJEICAO OPERACAO COR           *
      *===============================================================*
       01 DCOMV6-REGISTRO.
1         03 DCOMV6-COD-MSG            PIC  X(009).
2         03 DCOMV6-NRO-CTRLIF         PIC  X(020).
3         03 DCOMV6-CNPJ-ENT-RESP-X.
             05 DCOMV6-CNPJ-ENT-RESP   PIC  9(014).
      *      CONTEUDO -> REJEICOES - "N" REPETICOES
      *         - CODIGO DE ERRO SICOR - X(08)
      *         - DESCRICAO DO ERRO    - X(50)
          03 DCOMV6-CONTEUDO           PIC  X(2014)        VALUE SPACES.
          03 DCOMV6-DTHR-BC-X.
      *      FORMATO AAAAMMDDHHMMSS
             05 DCOMV6-DTHR-BC         PIC  9(014).
          03 DCOMV6-DATA-MOVTO-X.
      *      FORMATO AAAAMMDD
             05 DCOMV6-DATA-MOVTO      PIC  9(008).
