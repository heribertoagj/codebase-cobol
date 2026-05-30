      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  04-ARGUMENTOS-ENTRADA.

       05  04-COLUNAS-DA-TABELA.
           10  04-CFUNC-BDSCO-AUTRZ        PIC S9(9)V COMP-3.
           10  04-CPRODT                   PIC S9(3)V COMP-3.
           10  04-CTPO-AUTRZ-ESPCL         PIC S9(3)V COMP-3.
           10  04-DINIC-AUTRZ-ESPCL        PIC X(10).
           10  04-DFIM-AUTRZ-ESPCL         PIC X(10).
           10  04-HULT-ATULZ               PIC X(26).
           10  04-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  04-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  04-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  04-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  04-RETORNO.
       05  04-COD-RETORNO                         PIC 9(04).
       05  04-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  04-PROXIMO-RESTART                     PIC 9(05).
       05  04-QTDE-RETORNADA                      PIC 9(03).
       05  04-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  04-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T04.
           10  04RT-CFUNC-BDSCO-AUTRZ      PIC S9(9)V COMP-3.
           10  04RT-CPRODT                 PIC S9(3)V COMP-3.
           10  04RT-CTPO-AUTRZ-ESPCL       PIC S9(3)V COMP-3.
           10  04RT-DINIC-AUTRZ-ESPCL      PIC X(10).
           10  04RT-DFIM-AUTRZ-ESPCL       PIC X(10).
           10  04RT-HULT-ATULZ             PIC X(26).
           10  04RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  04RT-CTERM                  PIC X(8).
