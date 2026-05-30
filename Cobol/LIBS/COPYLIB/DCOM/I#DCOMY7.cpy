      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  Y7-ARGUMENTOS-ENTRADA.

         05  Y7-COLUNAS-DA-TABELA.
             10  Y7-CESPCE-TITLO-COBR        PIC S9(3)V       COMP-3.
             10  Y7-CSGL-ESPCE-TITLO         PIC X(03).
             10  Y7-RESPCE-TITLO-COBR        PIC X(40).
             10  Y7-CESPCE-TITLO-PROTE       PIC X(01).
             10  Y7-CACEIT-TITLO             PIC X(01).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
         05  Y7-QTDE-A-DESPREZAR           PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *----------------------------------------------*
         05  Y7-INSTRUCAO                  PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  Y7-RETORNO.
         05  Y7-COD-RETORNO                         PIC 9(04).
         05  Y7-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
         05  Y7-PROXIMO-RESTART                     PIC 9(05).
         05  Y7-QTDE-RETORNADA                      PIC 9(03).
         05  Y7-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
         05  Y7-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TY7.

           10  Y7RT-CESPCE-TITLO-COBR      PIC 9(03).
           10  Y7RT-CSGL-ESPCE-TITLO       PIC X(03).
           10  Y7RT-RESPCE-TITLO-COBR      PIC X(40).
           10  Y7RT-CESPCE-TITLO-PROTE     PIC X(01).
           10  Y7RT-CACEIT-TITLO           PIC X(01).
