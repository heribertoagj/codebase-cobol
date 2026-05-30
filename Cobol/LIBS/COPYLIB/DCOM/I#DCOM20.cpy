      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  20-ARGUMENTOS-ENTRADA.

       05  20-COLUNAS-DA-TABELA.
           10  20-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  20-WCHAVE-PRINC-ELMTO       PIC S9(9)V COMP-3.
           10  20-NULT-SEQ-ELMTO           PIC S9(15)V COMP-3.
           10  20-QDIG-SEQ-ELMTO           PIC S9(3)V COMP-3.
           10  20-HULT-ATULZ               PIC X(26).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  20-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  20-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  20-RETORNO.
       05  20-COD-RETORNO                         PIC 9(04).
       05  20-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  20-PROXIMO-RESTART                     PIC 9(05).
       05  20-QTDE-RETORNADA                      PIC 9(03).
       05  20-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  20-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T20.
           10  20RT-CELMTO-DESC-COML       PIC S9(3)V COMP-3.
           10  20RT-WCHAVE-PRINC-ELMTO     PIC S9(9)V COMP-3.
           10  20RT-NULT-SEQ-ELMTO         PIC S9(15)V COMP-3.
           10  20RT-QDIG-SEQ-ELMTO         PIC S9(3)V COMP-3.
           10  20RT-HULT-ATULZ             PIC X(26).
