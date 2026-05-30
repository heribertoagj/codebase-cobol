      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  74-ARGUMENTOS-ENTRADA.

       05  74-COLUNAS-DA-TABELA.
           10  74-CPRODT                   PIC S9(3)V COMP-3.
           10  74-DINIC-PARMZ-CONTR        PIC X(10).
           10  74-CTPO-COBR-TAC            PIC S9(3)V COMP-3.
           10  74-CTAC-CONTR-LIM           PIC S9(5)V COMP-3.
           10  74-CTAC-PERC-CONTR          PIC S9(5)V COMP-3.
           10  74-CTAC-MIN-CONTR           PIC S9(5)V COMP-3.
           10  74-CTAC-MAX-CONTR           PIC S9(5)V COMP-3.
           10  74-DFIM-PARMZ-CONTR         PIC X(10).
           10  74-HULT-ATULZ               PIC X(26).
           10  74-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  74-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  74-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  74-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  74-RETORNO.
       05  74-COD-RETORNO                         PIC 9(04).
       05  74-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  74-PROXIMO-RESTART                     PIC 9(05).
       05  74-QTDE-RETORNADA                      PIC 9(03).
       05  74-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  74-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T74.
           10  74RT-CPRODT                 PIC S9(3)V COMP-3.
           10  74RT-DINIC-PARMZ-CONTR      PIC X(10).
           10  74RT-CTPO-COBR-TAC          PIC S9(3)V COMP-3.
           10  74RT-CTAC-CONTR-LIM         PIC S9(5)V COMP-3.
           10  74RT-CTAC-PERC-CONTR        PIC S9(5)V COMP-3.
           10  74RT-CTAC-MIN-CONTR         PIC S9(5)V COMP-3.
           10  74RT-CTAC-MAX-CONTR         PIC S9(5)V COMP-3.
           10  74RT-DFIM-PARMZ-CONTR       PIC X(10).
           10  74RT-HULT-ATULZ             PIC X(26).
           10  74RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  74RT-CTERM                  PIC X(8).
