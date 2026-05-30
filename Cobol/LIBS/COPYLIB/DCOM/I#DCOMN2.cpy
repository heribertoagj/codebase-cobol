      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N2 - DB2PRD.TLIM_CLI_DESC          *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7640 - SELECT         - DCOM7641 - FETCH             *
      *    - DCOM7642 - INSERT         - DCOM7643 - UPDATE            *
      *    - DCOM7644 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N2-ARGUMENTOS-ENTRADA.

       05  N2-COLUNAS-DA-TABELA.
           10  N2-CBCO                 PIC S9(003) COMP-3.
           10  N2-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N2-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N2-CPRODT               PIC S9(003) COMP-3.
           10  N2-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N2-VLIM-DESC-COML       PIC S9(015)V9(2) COMP-3.
           10  N2-HINIC-VGCIA-DESC     PIC  X(026).
           10  N2-HFIM-VGCIA-DESC      PIC  X(026).
           10  N2-CTERM                PIC  X(008).
           10  N2-CUSUAR-INCL          PIC  X(009).
           10  N2-HINCL-REG            PIC  X(026).
           10  N2-CUSUAR-MANUT         PIC  X(009).
           10  N2-HATULZ               PIC  X(026).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N2-QTDE-A-DESPREZAR         PIC  9(05).
       05  N2-START-COUNT              PIC  X(01).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N2-INSTRUCAO                PIC  X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N2-RETORNO.
       05  N2-COD-RETORNO              PIC  9(04).
       05  N2-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N2-PROXIMO-RESTART          PIC  9(05).
       05  N2-QTDE-RETORNADA           PIC  9(03).
       05  N2-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN2.
           10  N2RT-CBCO                 PIC S9(003) COMP-3.
           10  N2RT-CAG-BCRIA            PIC S9(005) COMP-3.
           10  N2RT-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
           10  N2RT-CPRODT               PIC S9(003) COMP-3.
           10  N2RT-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N2RT-VLIM-DESC-COML       PIC S9(015)V9(2) COMP-3.
           10  N2RT-HINIC-VGCIA-DESC     PIC  X(026).
           10  N2RT-HFIM-VGCIA-DESC      PIC  X(026).
           10  N2RT-CTERM                PIC  X(008).
           10  N2RT-CUSUAR-INCL          PIC  X(009).
           10  N2RT-HINCL-REG            PIC  X(026).
           10  N2RT-CUSUAR-MANUT         PIC  X(009).
           10  N2RT-HATULZ               PIC  X(026).

