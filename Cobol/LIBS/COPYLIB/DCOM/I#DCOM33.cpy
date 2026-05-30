      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  33-ARGUMENTOS-ENTRADA.

       05  33-COLUNAS-DA-TABELA.
           10  33-CMEIO-ENTRD-DESC         PIC S9(3)V COMP-3.
           10  33-IMEIO-ENTRD-DESC         PIC X(25).
           10  33-IRSUMO-MEIO-ENTRD        PIC X(15).
           10  33-CSGL-MEIO-ENTRD          PIC X(6).
           10  33-HULT-ATULZ               PIC X(26).
           10  33-CTRANS-PROG-ATULZ        PIC X(8).
ADERBA* COLUNAS AUXILIARES
ADERBA     10  33-CPRODT                   PIC 9(3).
ADERBA     10  33-CSPROD-DESC-COML         PIC 9(3).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  33-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  33-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  33-RETORNO.
       05  33-COD-RETORNO                         PIC 9(04).
       05  33-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  33-PROXIMO-RESTART                     PIC 9(05).
       05  33-QTDE-RETORNADA                      PIC 9(03).
       05  33-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  33-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T33.
           10  33RT-CMEIO-ENTRD-DESC       PIC S9(3)V COMP-3.
           10  33RT-IMEIO-ENTRD-DESC       PIC X(25).
           10  33RT-IRSUMO-MEIO-ENTRD      PIC X(15).
           10  33RT-CSGL-MEIO-ENTRD        PIC X(6).
           10  33RT-HULT-ATULZ             PIC X(26).
           10  33RT-CTRANS-PROG-ATULZ      PIC X(8).
