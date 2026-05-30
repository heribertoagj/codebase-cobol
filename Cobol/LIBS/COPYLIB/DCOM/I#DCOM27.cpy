      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  27-ARGUMENTOS-ENTRADA.

       05  27-COLUNAS-DA-TABELA.
           10  27-CEVNTO-WORKF-DESC        PIC S9(3)V COMP-3.
           10  27-IEVNTO-WORKF-DESC        PIC X(40).
           10  27-IRSUMO-EVNTO-WORKF       PIC X(15).
           10  27-HULT-ATULZ               PIC X(26).
           10  27-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  27-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  27-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  27-RETORNO.
       05  27-COD-RETORNO                         PIC 9(04).
       05  27-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  27-PROXIMO-RESTART                     PIC 9(05).
       05  27-QTDE-RETORNADA                      PIC 9(03).
       05  27-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  27-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T27.
           10  27RT-CEVNTO-WORKF-DESC      PIC S9(3)V COMP-3.
           10  27RT-IEVNTO-WORKF-DESC      PIC X(40).
           10  27RT-IRSUMO-EVNTO-WORKF     PIC X(15).
           10  27RT-HULT-ATULZ             PIC X(26).
           10  27RT-CTRANS-PROG-ATULZ      PIC X(8).
