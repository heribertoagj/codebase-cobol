      *================================================================*
      * INCLUDE PARA TABELA: DCOMB0O0 - DB2PRD.TORIGE_PGTO_DESC        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7685 - SELECT         - DCOM7686 - FETCH              *
      *    - DCOM7687 - INSERT         - DCOM7688 - UPDATE             *
      *    - DCOM7689 - DELETE                                         *
      *================================================================*

       01  O0-ARGUMENTOS-ENTRADA.

       05  O0-COLUNAS-DA-TABELA.
           10  O0-CORIGE-PGTO-DESC     PIC S9(003) COMP-3.
           10  O0-IORIGE-PGTO-DESC     PIC  X(040).
           10  O0-HULT-ATULZ           PIC  X(026).
           10  O0-CFUNC-BDSCO          PIC S9(009) COMP-3.
           10  O0-CTERM                PIC  X(008).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  O0-QTDE-A-DESPREZAR          PIC  9(005).
       05  O0-START-COUNT               PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  O0-INSTRUCAO                 PIC  X(010).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  O0-RETORNO.
       05  O0-COD-RETORNO              PIC  9(004).
       05  O0-MENSAGEM                 PIC  X(079).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  O0-PROXIMO-RESTART          PIC  9(005).
       05  O0-QTDE-RETORNADA           PIC  9(003).
       05  O0-QTDE-TOTAL               PIC  9(009).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  O0-OCORRENCIAS  OCCURS 070 TIMES INDEXED BY IND-TO0.
           10  O0RT-CORIGE-PGTO-DESC   PIC S9(003) COMP-3.
           10  O0RT-IORIGE-PGTO-DESC   PIC  X(040).
           10  O0RT-HULT-ATULZ         PIC  X(026).
           10  O0RT-CFUNC-BDSCO        PIC S9(009) COMP-3.
           10  O0RT-CTERM              PIC  X(008).

