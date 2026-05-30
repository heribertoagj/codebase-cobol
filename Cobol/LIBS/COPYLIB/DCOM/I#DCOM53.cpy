      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  53-ARGUMENTOS-ENTRADA.

       05  53-COLUNAS-DA-TABELA.
           10  53-CRELAT-DESC-COML         PIC X(8).
           10  53-IRELAT-DESC-COML         PIC X(40).
           10  53-IRSUMO-RELAT-DESC        PIC X(20).
           10  53-CINDCD-SOLTC-EVTAL       PIC X(1).
           10  53-DULT-EMIS-RELAT          PIC X(10).
           10  53-HULT-ATULZ               PIC X(26).
           10  53-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  53-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  53-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  53-RETORNO.
       05  53-COD-RETORNO                         PIC 9(04).
       05  53-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  53-PROXIMO-RESTART                     PIC 9(05).
       05  53-QTDE-RETORNADA                      PIC 9(03).
       05  53-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  53-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T53.
           10  53RT-CRELAT-DESC-COML       PIC X(8).
           10  53RT-IRELAT-DESC-COML       PIC X(40).
           10  53RT-IRSUMO-RELAT-DESC      PIC X(20).
           10  53RT-CINDCD-SOLTC-EVTAL     PIC X(1).
           10  53RT-DULT-EMIS-RELAT        PIC X(10).
           10  53RT-HULT-ATULZ             PIC X(26).
           10  53RT-CTRANS-PROG-ATULZ      PIC X(8).
