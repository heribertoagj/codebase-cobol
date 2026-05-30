      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N3 - DB2PRD.TLIM_PRODT_DESC        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7645 - SELECT         - DCOM7646 - FETCH             *
      *    - DCOM7647 - INSERT         - DCOM7648 - UPDATE            *
      *    - DCOM7649 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N3-ARGUMENTOS-ENTRADA.

       05  N3-COLUNAS-DA-TABELA.
           10  N3-CPRODT               PIC S9(003) COMP-3.
           10  N3-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N3-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  N3-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  N3-VLIM-DESC-COML       PIC S9(015)V9(2) COMP-3.
           10  N3-HINIC-VGCIA-DESC     PIC  X(026).
           10  N3-HFIM-VGCIA-DESC      PIC  X(026).
           10  N3-CINDCD-LIM-OPER      PIC  X(001).
           10  N3-CTERM                PIC  X(008).
           10  N3-CUSUAR-INCL          PIC  X(009).
           10  N3-HINCL-REG            PIC  X(026).
           10  N3-CUSUAR-MANUT         PIC  X(009).
           10  N3-HATULZ               PIC  X(026).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N3-QTDE-A-DESPREZAR         PIC  9(05).
       05  N3-START-COUNT              PIC  X(01).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N3-INSTRUCAO                PIC  X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N3-RETORNO.
       05  N3-COD-RETORNO              PIC  9(04).
       05  N3-MENSAGEM                 PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N3-PROXIMO-RESTART          PIC  9(05).
       05  N3-QTDE-RETORNADA           PIC  9(03).
       05  N3-QTDE-TOTAL               PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N3-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN3.
           10  N3RT-CPRODT               PIC S9(003) COMP-3.
           10  N3RT-CSPROD-DESC-COML     PIC S9(003) COMP-3.
           10  N3RT-CSGMTO-CLI           PIC S9(003) COMP-3.
           10  N3RT-CORIGE-OPER-DESC     PIC S9(001) COMP-3.
           10  N3RT-VLIM-DESC-COML       PIC S9(015)V9(2) COMP-3.
           10  N3RT-HINIC-VGCIA-DESC     PIC  X(026).
           10  N3RT-HFIM-VGCIA-DESC      PIC  X(026).
           10  N3RT-CINDCD-LIM-OPER      PIC  X(001).
           10  N3RT-CTERM                PIC  X(008).
           10  N3RT-CUSUAR-INCL          PIC  X(009).
           10  N3RT-HINCL-REG            PIC  X(026).
           10  N3RT-CUSUAR-MANUT         PIC  X(009).
           10  N3RT-HATULZ               PIC  X(026).

