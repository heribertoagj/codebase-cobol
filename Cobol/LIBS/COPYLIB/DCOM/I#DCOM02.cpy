      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  02-ARGUMENTOS-ENTRADA.

       05  02-COLUNAS-DA-TABELA.
           10  02-CABRGC-ISENC-DESC        PIC S9(3)V COMP-3.
           10  02-IABRGC-ISENC-DESC        PIC X(40).
           10  02-IRSUMO-ABRGC-ISENC       PIC X(15).
           10  02-HULT-ATULZ               PIC X(26).
           10  02-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  02-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  02-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  02-RETORNO.
       05  02-COD-RETORNO                         PIC 9(04).
       05  02-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  02-PROXIMO-RESTART                     PIC 9(05).
       05  02-QTDE-RETORNADA                      PIC 9(03).
       05  02-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  02-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T02.
           10  02RT-CABRGC-ISENC-DESC      PIC S9(3)V COMP-3.
           10  02RT-IABRGC-ISENC-DESC      PIC X(40).
           10  02RT-IRSUMO-ABRGC-ISENC     PIC X(15).
           10  02RT-HULT-ATULZ             PIC X(26).
           10  02RT-CTRANS-PROG-ATULZ      PIC X(8).
