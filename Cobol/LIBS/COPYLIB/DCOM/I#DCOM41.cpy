      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  41-ARGUMENTOS-ENTRADA.

       05  41-COLUNAS-DA-TABELA.
           10  41-CPERDC-DESC-COML         PIC X(3).
           10  41-IPERDC-DESC-COML         PIC X(15).
           10  41-HULT-ATULZ               PIC X(26).
           10  41-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  41-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  41-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  41-RETORNO.
       05  41-COD-RETORNO                         PIC 9(04).
       05  41-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  41-PROXIMO-RESTART                     PIC 9(05).
       05  41-QTDE-RETORNADA                      PIC 9(03).
       05  41-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  41-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T41.
           10  41RT-CPERDC-DESC-COML       PIC X(3).
           10  41RT-IPERDC-DESC-COML       PIC X(15).
           10  41RT-HULT-ATULZ             PIC X(26).
           10  41RT-CTRANS-PROG-ATULZ      PIC X(8).
