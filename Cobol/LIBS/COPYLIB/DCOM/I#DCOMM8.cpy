      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M8 - DB2PRD.TREGRA_CTBIL_PREJ                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7620 - SELECT         - DCOM7621 - FETCH             *
      *    - DCOM7622 - INSERT         - DCOM7623 - UPDATE            *
      *    - DCOM7624 - DELETE                                        *
      *===============================================================*

       01  M8-ARGUMENTOS-ENTRADA.

       05  M8-COLUNAS-DA-TABELA.
           10  M8-CMOTVO-BAIXA-PREJ    PIC S9(003)        COMP-3.
           10  M8-CREGRA-CTBIL-PREJ    PIC S9(003)        COMP-3.
           10  M8-IREGRA-CTBIL-PREJ    PIC  X(030).
           10  M8-HULT-ATULZ           PIC  X(026).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M8-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M8-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M8-RETORNO.
       05  M8-COD-RETORNO              PIC  9(004).
       05  M8-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M8-PROXIMO-RESTART          PIC  9(005).
       05  M8-QTDE-RETORNADA           PIC  9(003).
       05  M8-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M8-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM8.
           10  M8RT-CMOTVO-BAIXA-PREJ    PIC S9(003)        COMP-3.
           10  M8RT-CREGRA-CTBIL-PREJ    PIC S9(003)        COMP-3.
           10  M8RT-IREGRA-CTBIL-PREJ    PIC  X(030).
           10  M8RT-HULT-ATULZ           PIC  X(026).

