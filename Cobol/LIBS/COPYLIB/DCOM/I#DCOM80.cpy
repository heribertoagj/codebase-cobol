      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  80-ARGUMENTOS-ENTRADA.

       05  80-COLUNAS-DA-TABELA.
           10  80-CTPO-AUTRZ-ESPCL         PIC S9(3)V COMP-3.
           10  80-ITPO-AUTRZ-ESPCL         PIC X(30).
           10  80-IRSUMO-AUTRZ-ESPCL       PIC X(15).
           10  80-CTRANS-PROG-ATULZ        PIC X(08).
           10  80-CINDCD-SELEC-AUTRZ       PIC X(01).
           10  80-HULT-ATULZ               PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  80-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  80-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  80-RETORNO.
       05  80-COD-RETORNO                         PIC 9(04).
       05  80-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  80-PROXIMO-RESTART                     PIC 9(05).
       05  80-QTDE-RETORNADA                      PIC 9(03).
       05  80-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  80-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T80.
           10  80RT-CTPO-AUTRZ-ESPCL         PIC S9(3)V COMP-3.
           10  80RT-ITPO-AUTRZ-ESPCL         PIC X(30).
           10  80RT-IRSUMO-AUTRZ-ESPCL       PIC X(15).
           10  80RT-CTRANS-PROG-ATULZ        PIC X(08).
           10  80RT-CINDCD-SELEC-AUTRZ       PIC X(01).
           10  80RT-HULT-ATULZ               PIC X(26).
