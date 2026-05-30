      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  82-ARGUMENTOS-ENTRADA.

       05  82-COLUNAS-DA-TABELA.
           10  82-CCTA-CONC-CTBIL          PIC S9(3)V COMP-3.
           10  82-ITPO-CONC-CTBIL          PIC X(30).
           10  82-IRSUMO-CTA-CONC          PIC X(10).
           10  82-CSGL-CTA-CONC            PIC X(6).
           10  82-HULT-ATULZ               PIC X(26).
           10  82-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  82-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  82-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  82-RETORNO.
       05  82-COD-RETORNO                         PIC 9(04).
       05  82-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  82-PROXIMO-RESTART                     PIC 9(05).
       05  82-QTDE-RETORNADA                      PIC 9(03).
       05  82-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  82-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T82.
           10  82RT-CCTA-CONC-CTBIL        PIC S9(3)V COMP-3.
           10  82RT-ITPO-CONC-CTBIL        PIC X(30).
           10  82RT-IRSUMO-CTA-CONC        PIC X(10).
           10  82RT-CSGL-CTA-CONC          PIC X(6).
           10  82RT-HULT-ATULZ             PIC X(26).
           10  82RT-CTRANS-PROG-ATULZ      PIC X(8).
