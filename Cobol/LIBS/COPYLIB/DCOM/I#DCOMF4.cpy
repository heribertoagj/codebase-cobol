      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F4 - DB2PRD.TTRASI_CART_COBR       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F4-ARGUMENTOS-ENTRADA.

       05  F4-COLUNAS-DA-TABELA.
           10  F4-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  F4-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  F4-CBCO-PRODT-COBR              PIC S9(3)V COMP-3.
           10  F4-CIDTFD-PRODT-COBR            PIC S9(2)V COMP-3.
           10  F4-CNEGOC-COBR                  PIC S9(18)V COMP-3.
           10  F4-CINDCD-SELEC-DESC            PIC X(1).
           10  F4-HULT-ATULZ                   PIC X(26).
           10  F4-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  F4-CTERM                        PIC X(8).
           10  F4-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F4-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F4-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F4-RETORNO.
       05  F4-COD-RETORNO                         PIC 9(04).
       05  F4-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F4-PROXIMO-RESTART                     PIC 9(05).
       05  F4-QTDE-RETORNADA                      PIC 9(03).
       05  F4-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F4-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF4.
           10  F4RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  F4RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  F4RT-CBCO-PRODT-COBR            PIC S9(3)V COMP-3.
           10  F4RT-CIDTFD-PRODT-COBR          PIC S9(2)V COMP-3.
           10  F4RT-CNEGOC-COBR                PIC S9(18)V COMP-3.
           10  F4RT-CINDCD-SELEC-DESC          PIC X(1).
           10  F4RT-HULT-ATULZ                 PIC X(26).
           10  F4RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  F4RT-CTERM                      PIC X(8).
           10  F4RT-DINCL-REG                  PIC X(10).
