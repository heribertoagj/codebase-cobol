      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N0 - DB2PRD.THIST_LIM_CLI          *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7630 - SELECT         - DCOM7631 - FETCH             *
      *    - DCOM7632 - INSERT         - DCOM7633 - UPDATE            *
      *    - DCOM7634 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N0-ARGUMENTOS-ENTRADA.

       05  N0-COLUNAS-DA-TABELA.
           10  N0-CBCO                 PIC S9(003) COMP-3.
           10  N0-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N0-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N0-CPRODT               PIC S9(003) COMP-3.
           10  N0-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N0-HINCL-REG            PIC  X(026).
           10  N0-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N0-VLIM-DESC-COML       PIC S9(015)V9(2) COMP-3.
           10  N0-HINIC-VGCIA-DESC     PIC  X(026).
           10  N0-HFIM-VGCIA-DESC      PIC  X(026).
           10  N0-CTERM                PIC  X(008).
           10  N0-CUSUAR-INCL          PIC  X(009).
           10  N0-CUSUAR-MANUT         PIC  X(009).
           10  N0-HATULZ               PIC  X(026).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N0-QTDE-A-DESPREZAR         PIC  9(05).
       05  N0-START-COUNT              PIC  X(01).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N0-INSTRUCAO                PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N0-RETORNO.
       05  N0-COD-RETORNO              PIC 9(04).
       05  N0-MENSAGEM                 PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N0-PROXIMO-RESTART          PIC 9(05).
       05  N0-QTDE-RETORNADA           PIC 9(03).
       05  N0-QTDE-TOTAL               PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N0-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN0.
           10  N0RT-CBCO                 PIC S9(003) COMP-3.
           10  N0RT-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N0RT-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N0RT-CPRODT               PIC S9(003) COMP-3.
           10  N0RT-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N0RT-HINCL-REG            PIC  X(026).
           10  N0RT-CIDTFD-TPO-MANUT     PIC  X(001).
           10  N0RT-VLIM-DESC-COML       PIC S9(015)V9(2) COMP-3.
           10  N0RT-HINIC-VGCIA-DESC     PIC  X(026).
           10  N0RT-HFIM-VGCIA-DESC      PIC  X(026).
           10  N0RT-CTERM                PIC  X(008).
           10  N0RT-CUSUAR-INCL          PIC  X(009).
           10  N0RT-CUSUAR-MANUT         PIC  X(009).
           10  N0RT-HATULZ               PIC  X(026).

