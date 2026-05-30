      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0F7 - DB2PRD.TTRASI_CONDC_PGTO      *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  F7-ARGUMENTOS-ENTRADA.

       05  F7-COLUNAS-DA-TABELA.
           10  F7-DANO-OPER-DESC               PIC S9(4)V COMP-3.
           10  F7-NSEQ-OPER-DESC               PIC S9(9)V COMP-3.
           10  F7-CCONDC-PGTO-FORNC            PIC S9(5)V COMP-3.
           10  F7-CINDCD-SELEC-DESC            PIC X(1).
           10  F7-HULT-ATULZ                   PIC X(26).
           10  F7-CFUNC-BDSCO                  PIC S9(9)V COMP-3.
           10  F7-CTERM                        PIC X(8).
           10  F7-DINCL-REG                    PIC X(10).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  F7-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  F7-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  F7-RETORNO.
       05  F7-COD-RETORNO                         PIC 9(04).
       05  F7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  F7-PROXIMO-RESTART                     PIC 9(05).
       05  F7-QTDE-RETORNADA                      PIC 9(03).
       05  F7-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  F7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TF7.
           10  F7RT-DANO-OPER-DESC             PIC S9(4)V COMP-3.
           10  F7RT-NSEQ-OPER-DESC             PIC S9(9)V COMP-3.
           10  F7RT-CCONDC-PGTO-FORNC          PIC S9(5)V COMP-3.
           10  F7RT-CINDCD-SELEC-DESC          PIC X(1).
           10  F7RT-HULT-ATULZ                 PIC X(26).
           10  F7RT-CFUNC-BDSCO                PIC S9(9)V COMP-3.
           10  F7RT-CTERM                      PIC X(8).
           10  F7RT-DINCL-REG                  PIC X(10).
