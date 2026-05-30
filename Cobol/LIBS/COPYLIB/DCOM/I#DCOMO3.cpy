      *================================================================*
      * INCLUDE PARA TABELA: DCOMB0O3 - DB2PRD.TSMULA_AGNDA_CATAO      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7700 - SELECT         - DCOM7701 - FETCH              *
      *    - DCOM7702 - INSERT         - DCOM7703 - UPDATE             *
      *    - DCOM7704 - DELETE                                         *
      *================================================================*

       01  O3-ARGUMENTOS-ENTRADA.

       05  O3-COLUNAS-DA-TABELA.
           10  O3-NSMULA-AGNDA-CATAO   PIC S9(009)        COMP-3.
           10  O3-CBCO                 PIC S9(003)        COMP-3.
           10  O3-CAG-BCRIA            PIC S9(005)        COMP-3.
           10  O3-CCTA-BCRIA-CLI       PIC S9(013)        COMP-3.
           10  O3-CIDTFD-PRODT-CATAO   PIC S9(005)        COMP-3.
           10  O3-CSIT-DESC-COML       PIC S9(003)        COMP-3.
           10  O3-CSELEC-AGNDA-CATAO   PIC  X(001).
           10  O3-CIDTFD-TPO-MANUT     PIC  X(001).
           10  O3-CIDTFD-TPO-AGNDA     PIC  X(001).
           10  O3-RJUSTF-AGNDA-CATAO   PIC  X(040).
           10  O3-PAGNDA-ANTEC-CATAO   PIC S9(003)V9(002) COMP-3.
           10  O3-VAGNDA-ANTEC-CATAO   PIC S9(015)V9(002) COMP-3.
           10  O3-HINCL-REG            PIC  X(026).
           10  O3-HULT-ATULZ           PIC  X(026).
           10  O3-CFUNC-BDSCO          PIC S9(009)        COMP-3.
           10  O3-CTERM                PIC  X(008).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  O3-QTDE-A-DESPREZAR         PIC  9(005).
       05  O3-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  O3-INSTRUCAO                PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  O3-RETORNO.
       05  O3-COD-RETORNO              PIC  9(004).
       05  O3-MENSAGEM                 PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  O3-PROXIMO-RESTART          PIC  9(005).
       05  O3-QTDE-RETORNADA           PIC  9(003).
       05  O3-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  O3-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TO3.
           10  O3RT-NSMULA-AGNDA-CATAO PIC S9(009)        COMP-3.
           10  O3RT-CBCO               PIC S9(003)        COMP-3.
           10  O3RT-CAG-BCRIA          PIC S9(005)        COMP-3.
           10  O3RT-CCTA-BCRIA-CLI     PIC S9(013)        COMP-3.
           10  O3RT-CIDTFD-PRODT-CATAO PIC S9(005)        COMP-3.
           10  O3RT-CSIT-DESC-COML     PIC S9(003)        COMP-3.
           10  O3RT-CSELEC-AGNDA-CATAO PIC  X(001).
           10  O3RT-CIDTFD-TPO-MANUT   PIC  X(001).
           10  O3RT-CIDTFD-TPO-AGNDA   PIC  X(001).
           10  O3RT-RJUSTF-AGNDA-CATAO PIC  X(040).
           10  O3RT-PAGNDA-ANTEC-CATAO PIC S9(003)V9(002) COMP-3.
           10  O3RT-VAGNDA-ANTEC-CATAO PIC S9(015)V9(002) COMP-3.
           10  O3RT-HINCL-REG          PIC  X(026).
           10  O3RT-HULT-ATULZ         PIC  X(026).
           10  O3RT-CFUNC-BDSCO        PIC S9(009)        COMP-3.
           10  O3RT-CTERM              PIC  X(008).

