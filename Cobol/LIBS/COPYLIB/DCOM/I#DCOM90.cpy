      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB090 - DB2PRD.TMEIO_PGTO_DESC        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  90-ARGUMENTOS-ENTRADA.

       05  90-COLUNAS-DA-TABELA.
           10  90-CMEIO-PGTO-DESC              PIC S9(3)V COMP-3.
           10  90-IMEIO-PGTO-DESC              PIC X(40).
           10  90-IRSUMO-MEIO-PGTO             PIC X(15).
           10  90-CSGL-MEIO-PGTO               PIC X(6).
           10  90-CINDCD-MEIO-CNDDO            PIC X(1).
           10  90-HULT-ATULZ                   PIC X(26).
           10  90-CTRANS-PROG-ATULZ            PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  90-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  90-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  90-RETORNO.
       05  90-COD-RETORNO                         PIC 9(04).
       05  90-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  90-PROXIMO-RESTART                     PIC 9(05).
       05  90-QTDE-RETORNADA                      PIC 9(03).
       05  90-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  90-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T90.
           10  90RT-CMEIO-PGTO-DESC            PIC S9(3)V COMP-3.
           10  90RT-IMEIO-PGTO-DESC            PIC X(40).
           10  90RT-IRSUMO-MEIO-PGTO           PIC X(15).
           10  90RT-CSGL-MEIO-PGTO             PIC X(6).
           10  90RT-CINDCD-MEIO-CNDDO          PIC X(1).
           10  90RT-HULT-ATULZ                 PIC X(26).
           10  90RT-CTRANS-PROG-ATULZ          PIC X(8).
