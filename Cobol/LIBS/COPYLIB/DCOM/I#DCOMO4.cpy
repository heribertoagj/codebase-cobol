      *================================================================*
      * INCLUDE PARA TABELA: DCOMB0O4 - DB2PRD.TAGNDA_ANTEC_CATAO      *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                           *
      *    - DCOM7705 - SELECT         - DCOM7706 - FETCH              *
      *    - DCOM7707 - INSERT         - DCOM7708 - UPDATE             *
      *    - DCOM7709 - DELETE                                         *
      *================================================================*

       01  O4-ARGUMENTOS-ENTRADA.

       05  O4-COLUNAS-DA-TABELA.
           10  O4-CBCO                 PIC S9(003)        COMP-3.
           10  O4-CAG-BCRIA            PIC S9(005)        COMP-3.
           10  O4-CCTA-BCRIA-CLI       PIC S9(013)        COMP-3.
           10  O4-CIDTFD-PRODT-CATAO   PIC S9(005)        COMP-3.
           10  O4-CIDTFD-TPO-MANUT     PIC  X(001).
           10  O4-CIDTFD-TPO-AGNDA     PIC  X(001).
           10  O4-RJUSTF-AGNDA-CATAO   PIC  X(040).
           10  O4-PAGNDA-ANTEC-CATAO   PIC S9(003)V9(002) COMP-3.
           10  O4-VAGNDA-ANTEC-CATAO   PIC S9(015)V9(002) COMP-3.
           10  O4-CORIGE-ANTEC-PGMD    PIC S9(001)        COMP-3.
           10  O4-HINCL-REG            PIC  X(026).
           10  O4-HATULZ               PIC  X(026).
           10  O4-CFUNC-BDSCO          PIC S9(009)        COMP-3.
           10  O4-CTERM                PIC  X(008).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  O4-QTDE-A-DESPREZAR         PIC  9(005).
       05  O4-START-COUNT              PIC  X(001).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  O4-INSTRUCAO                PIC  X(010).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  O4-RETORNO.
       05  O4-COD-RETORNO              PIC  9(004).
       05  O4-MENSAGEM                 PIC  X(079).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  O4-PROXIMO-RESTART          PIC  9(005).
       05  O4-QTDE-RETORNADA           PIC  9(003).
       05  O4-QTDE-TOTAL               PIC  9(009).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  O4-OCORRENCIAS   OCCURS 070 TIMES INDEXED BY IND-TO4.
           10  O4RT-CBCO               PIC S9(003)        COMP-3.
           10  O4RT-CAG-BCRIA          PIC S9(005)        COMP-3.
           10  O4RT-CCTA-BCRIA-CLI     PIC S9(013)        COMP-3.
           10  O4RT-CIDTFD-PRODT-CATAO PIC S9(005)        COMP-3.
           10  O4RT-CIDTFD-TPO-MANUT   PIC  X(001).
           10  O4RT-CIDTFD-TPO-AGNDA   PIC  X(001).
           10  O4RT-RJUSTF-AGNDA-CATAO PIC  X(040).
           10  O4RT-PAGNDA-ANTEC-CATAO PIC S9(003)V9(002) COMP-3.
           10  O4RT-VAGNDA-ANTEC-CATAO PIC S9(015)V9(002) COMP-3.
           10  O4RT-CORIGE-ANTEC-PGMD  PIC S9(001)        COMP-3.
           10  O4RT-HINCL-REG          PIC  X(026).
           10  O4RT-HATULZ             PIC  X(026).
           10  O4RT-CFUNC-BDSCO        PIC S9(009)        COMP-3.
           10  O4RT-CTERM              PIC  X(008).

