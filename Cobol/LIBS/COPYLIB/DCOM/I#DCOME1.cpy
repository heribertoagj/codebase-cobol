      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0E1 - DB2PRD.TSMULA_CONDC_PGTO      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  E1-ARGUMENTOS-ENTRADA.

       05  E1-COLUNAS-DA-TABELA.
           10  E1-NSMULA-OPER-DESC             PIC S9(9)V COMP-3.
           10  E1-CCONDC-PGTO-FORNC            PIC S9(5)V COMP-3.
           10  E1-CINDCD-SELEC-DESC            PIC X(1).
           10  E1-HULT-ATULZ                   PIC X(26).
           10  E1-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  E1-CTERM                        PIC X(8).
           10  E1-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  E1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  E1-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  E1-RETORNO.
       05  E1-COD-RETORNO                         PIC 9(04).
       05  E1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  E1-PROXIMO-RESTART                     PIC 9(05).
       05  E1-QTDE-RETORNADA                      PIC 9(03).
       05  E1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  E1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TE1.
           10  E1RT-NSMULA-OPER-DESC           PIC S9(9)V COMP-3.
           10  E1RT-CCONDC-PGTO-FORNC          PIC S9(5)V COMP-3.
           10  E1RT-CINDCD-SELEC-DESC          PIC X(1).
           10  E1RT-HULT-ATULZ                 PIC X(26).
           10  E1RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  E1RT-CTERM                      PIC X(8).
           10  E1RT-DINCL-REG                  PIC X(10).
