      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  J5-ARGUMENTOS-ENTRADA.

       05  J5-COLUNAS-DA-TABELA.
           10  J5-CBCO                     PIC S9(3)V COMP-3.
           10  J5-CAG-BCRIA                PIC S9(5)V COMP-3.
           10  J5-CCTA-BCRIA-CLI           PIC S9(13)V COMP-3.
           10  J5-NULT-SEQ-CONTR           PIC S9(18)V COMP-3.
           10  J5-QDIG-SEQ-CONTR           PIC S9(7)V COMP-3.
           10  J5-HULT-ATULZ               PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  J5-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  J5-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  J5-RETORNO.
       05  J5-COD-RETORNO                         PIC 9(04).
       05  J5-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  J5-PROXIMO-RESTART                     PIC 9(05).
       05  J5-QTDE-RETORNADA                      PIC 9(03).
       05  J5-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  J5-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJ5.
           10  J5RT-CBCO                     PIC S9(3)V COMP-3.
           10  J5RT-CAG-BCRIA                PIC S9(5)V COMP-3.
           10  J5RT-CCTA-BCRIA-CLI           PIC S9(13)V COMP-3.
           10  J5RT-NULT-SEQ-CONTR           PIC S9(18)V COMP-3.
           10  J5RT-QDIG-SEQ-CONTR           PIC S9(7)V COMP-3.
           10  J5RT-HULT-ATULZ               PIC X(26).
