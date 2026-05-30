      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0N1 - DB2PRD.THIST_LIM_PRODT        *
      *---------------------------------------------------------------*
      * MODULOS DE ACESSO..:                                          *
      *    - DCOM7635 - SELECT         - DCOM7636 - FETCH             *
      *    - DCOM7637 - INSERT         - DCOM7638 - UPDATE            *
      *    - DCOM7639 - DELETE                                        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  N1-ARGUMENTOS-ENTRADA.

       05  N1-COLUNAS-DA-TABELA.
           10  N1-CPRODT                PIC S9(003) COMP-3.
           10  N1-CSPROD-DESC-COML      PIC S9(003) COMP-3.
           10  N1-CSGMTO-CLI            PIC S9(003) COMP-3.
           10  N1-CORIGE-OPER-DESC      PIC S9(001) COMP-3.
           10  N1-HINCL-REG             PIC  X(026).
           10  N1-CIDTFD-TPO-MANUT      PIC  X(001).
           10  N1-VLIM-DESC-COML        PIC S9(015)V9(2) COMP-3.
           10  N1-HINIC-VGCIA-DESC      PIC  X(026).
           10  N1-HFIM-VGCIA-DESC       PIC  X(026).
           10  N1-CINDCD-LIM-OPER       PIC  X(001).
           10  N1-CTERM                 PIC  X(008).
           10  N1-CUSUAR-INCL           PIC  X(009).
           10  N1-CUSUAR-MANUT          PIC  X(009).
           10  N1-HATULZ                PIC  X(026).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  N1-QTDE-A-DESPREZAR          PIC  9(05).
       05  N1-START-COUNT               PIC  X(01).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  N1-INSTRUCAO                 PIC  X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  N1-RETORNO.
       05  N1-COD-RETORNO               PIC  9(04).
       05  N1-MENSAGEM                  PIC  X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  N1-PROXIMO-RESTART           PIC  9(05).
       05  N1-QTDE-RETORNADA            PIC  9(03).
       05  N1-QTDE-TOTAL                PIC  9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  N1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TN1.
           10  N1RT-CPRODT                PIC S9(003) COMP-3.
           10  N1RT-CSPROD-DESC-COML      PIC S9(003) COMP-3.
           10  N1RT-CSGMTO-CLI            PIC S9(003) COMP-3.
           10  N1RT-CORIGE-OPER-DESC      PIC S9(001) COMP-3.
           10  N1RT-HINCL-REG             PIC  X(026).
           10  N1RT-CIDTFD-TPO-MANUT      PIC  X(001).
           10  N1RT-VLIM-DESC-COML        PIC S9(015)V9(2) COMP-3.
           10  N1RT-HINIC-VGCIA-DESC      PIC  X(026).
           10  N1RT-HFIM-VGCIA-DESC       PIC  X(026).
           10  N1RT-CINDCD-LIM-OPER       PIC  X(001).
           10  N1RT-CTERM                 PIC  X(008).
           10  N1RT-CUSUAR-INCL           PIC  X(009).
           10  N1RT-CUSUAR-MANUT          PIC  X(009).
           10  N1RT-HATULZ                PIC  X(026).

