      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  26-ARGUMENTOS-ENTRADA.

       05  26-COLUNAS-DA-TABELA.
           10  26-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  26-IEVNTO-DESC-COML         PIC X(40).
           10  26-IRSUMO-EVNTO-DESC        PIC X(20).
           10  26-CTRANS-PROG-ATULZ        PIC X(8).
           10  26-HULT-ATULZ               PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  26-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  26-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  26-RETORNO.
       05  26-COD-RETORNO                         PIC 9(04).
       05  26-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  26-PROXIMO-RESTART                     PIC 9(05).
       05  26-QTDE-RETORNADA                      PIC 9(03).
       05  26-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  26-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T26.
           10  26RT-CEVNTO-DESC-COML       PIC S9(3)V COMP-3.
           10  26RT-IEVNTO-DESC-COML       PIC X(40).
           10  26RT-IRSUMO-EVNTO-DESC      PIC X(20).
           10  26RT-CTRANS-PROG-ATULZ      PIC X(8).
           10  26RT-HULT-ATULZ             PIC X(26).
