      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  57-ARGUMENTOS-ENTRADA.

       05  57-COLUNAS-DA-TABELA.
           10  57-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  57-ISIT-DESC-COML           PIC X(30).
           10  57-IRSUMO-SIT-DESC          PIC X(15).
           10  57-CSGL-SIT-DESC            PIC X(6).
           10  57-HULT-ATULZ               PIC X(26).
           10  57-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  57-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  57-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  57-RETORNO.
       05  57-COD-RETORNO                         PIC 9(04).
       05  57-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  57-PROXIMO-RESTART                     PIC 9(05).
       05  57-QTDE-RETORNADA                      PIC 9(03).
       05  57-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  57-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T57.
           10  57RT-CSIT-DESC-COML         PIC S9(3)V COMP-3.
           10  57RT-ISIT-DESC-COML         PIC X(30).
           10  57RT-IRSUMO-SIT-DESC        PIC X(15).
           10  57RT-CSGL-SIT-DESC          PIC X(6).
           10  57RT-HULT-ATULZ             PIC X(26).
           10  57RT-CTRANS-PROG-ATULZ      PIC X(8).
