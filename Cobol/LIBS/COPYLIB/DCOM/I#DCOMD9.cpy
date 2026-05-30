      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0D9 - DB2PRD.TSMULA_CART_COBR       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  D9-ARGUMENTOS-ENTRADA.

       05  D9-COLUNAS-DA-TABELA.
           10  D9-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  D9-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  D9-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  D9-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  D9-CINDCD-SELEC-DESC            PIC X(1).
           10  D9-HULT-ATULZ                   PIC X(26).
           10  D9-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  D9-CTERM                        PIC X(8).
           10  D9-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  D9-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  D9-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  D9-RETORNO.
       05  D9-COD-RETORNO                         PIC 9(04).
       05  D9-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  D9-PROXIMO-RESTART                     PIC 9(05).
       05  D9-QTDE-RETORNADA                      PIC 9(03).
       05  D9-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  D9-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TD9.
           10  D9RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  D9RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  D9RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  D9RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  D9RT-CINDCD-SELEC-DESC          PIC X(1).
           10  D9RT-HULT-ATULZ                 PIC X(26).
           10  D9RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  D9RT-CTERM                      PIC X(8).
           10  D9RT-DINCL-REG                  PIC X(10).
