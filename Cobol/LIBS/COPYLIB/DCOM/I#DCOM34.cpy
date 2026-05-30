      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  34-ARGUMENTOS-ENTRADA.

       05  34-COLUNAS-DA-TABELA.
           10  34-CMEIO-LIBRC-DESC         PIC S9(3)V COMP-3.
           10  34-IMEIO-LIBRC-DESC         PIC X(25).
           10  34-IRSUMO-MEIO-LIBRC        PIC X(15).
           10  34-CSGL-MEIO-LIBRC          PIC X(6).
           10  34-HULT-ATULZ               PIC X(26).
           10  34-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  34-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  34-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  34-RETORNO.
       05  34-COD-RETORNO                         PIC 9(04).
       05  34-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  34-PROXIMO-RESTART                     PIC 9(05).
       05  34-QTDE-RETORNADA                      PIC 9(03).
       05  34-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  34-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T34.
           10  34RT-CMEIO-LIBRC-DESC       PIC S9(3)V COMP-3.
           10  34RT-IMEIO-LIBRC-DESC       PIC X(25).
           10  34RT-IRSUMO-MEIO-LIBRC      PIC X(15).
           10  34RT-CSGL-MEIO-LIBRC        PIC X(6).
           10  34RT-HULT-ATULZ             PIC X(26).
           10  34RT-CTRANS-PROG-ATULZ      PIC X(8).
