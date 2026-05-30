      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E2 - DB2PRD.TSMULA_DIGTC_DESC      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E2-ARGUMENTOS-ENTRADA.

       05  E2-COLUNAS-DA-TABELA.
           10  E2-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  E2-NSMULA-DIGTC-DESC            PIC S9(5)V COMP-3.
           10  E2-DFLUXO-SMULA-DIGTC           PIC X(10).
           10  E2-VFLUXO-SMULA-DIGTC           PIC S9(15)V9(2) COMP-3.
           10  E2-TFLUXO-SMULA-DIGTC           PIC S9(5)V COMP-3.
           10  E2-HULT-ATULZ                   PIC X(26).
           10  E2-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E2-CTERM                        PIC X(8).
           10  E2-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E2-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E2-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E2-RETORNO.
       05  E2-COD-RETORNO                         PIC 9(04).
       05  E2-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E2-PROXIMO-RESTART                     PIC 9(05).
       05  E2-QTDE-RETORNADA                      PIC 9(03).
       05  E2-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E2-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE2.
           10  E2RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E2RT-NSMULA-DIGTC-DESC          PIC S9(5)V COMP-3.
           10  E2RT-DFLUXO-SMULA-DIGTC         PIC X(10).
           10  E2RT-VFLUXO-SMULA-DIGTC         PIC S9(15)V9(2) COMP-3.
           10  E2RT-TFLUXO-SMULA-DIGTC         PIC S9(5)V COMP-3.
           10  E2RT-HULT-ATULZ                 PIC X(26).
           10  E2RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E2RT-CTERM                      PIC X(8).
           10  E2RT-DINCL-REG                  PIC X(10).
