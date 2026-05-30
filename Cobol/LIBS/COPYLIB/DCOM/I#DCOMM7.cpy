      *===============================================================*
      * INCLUDE PARA TABELA:                                          *
      *    - DCOMB0M7 - DB2PRD.TMOTVO_BAIXA_PREJ                       *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7615 - SELECT         - DCOM7616 - FETCH             *
      *    - DCOM7617 - INSERT         - DCOM7618 - UPDATE            *
      *    - DCOM7619 - DELETE                                        *
      *===============================================================*

       01  M7-ARGUMENTOS-ENTRADA.

       05  M7-COLUNAS-DA-TABELA.
           10  M7-CMOTVO-BAIXA-PREJ    PIC S9(003)        COMP-3.
           10  M7-RMOTVO-BAIXA-PREJ    PIC  X(030).
           10  M7-HULT-ATULZ           PIC  X(026).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  M7-QTDE-A-DESPREZAR         PIC  9(005).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  M7-INSTRUCAO                PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  M7-RETORNO.
       05  M7-COD-RETORNO              PIC  9(004).
       05  M7-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  M7-PROXIMO-RESTART          PIC  9(005).
       05  M7-QTDE-RETORNADA           PIC  9(003).
       05  M7-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  M7-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TM7.
           10  M7RT-CMOTVO-BAIXA-PREJ  PIC S9(003)        COMP-3.
           10  M7RT-RMOTVO-BAIXA-PREJ  PIC  X(030).
           10  M7RT-HULT-ATULZ         PIC  X(026).

