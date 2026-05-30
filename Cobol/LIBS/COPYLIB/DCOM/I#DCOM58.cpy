      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  58-ARGUMENTOS-ENTRADA.

       05  58-COLUNAS-DA-TABELA.
           10  58-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  58-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  58-RSIT-ELMTO-DESC.
              49  58-RSIT-ELMTO-DESC-LEN   PIC S9(4) COMP.
              49  58-RSIT-ELMTO-DESC-TEXT  PIC X(240).
           10  58-TEXPUR-ELMTO-DESC        PIC S9(5)V COMP-3.
           10  58-CINDCD-PRZ-EXPUR         PIC X(2).
           10  58-HULT-ATULZ               PIC X(26).
           10  58-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  58-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  58-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  58-RETORNO.
       05  58-COD-RETORNO                         PIC 9(04).
       05  58-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  58-PROXIMO-RESTART                     PIC 9(05).
       05  58-QTDE-RETORNADA                      PIC 9(03).
       05  58-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  58-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T58.
           10  58RT-CELMTO-DESC-COML        PIC S9(3)V COMP-3.
           10  58RT-CSIT-DESC-COML          PIC S9(3)V COMP-3.
           10  58RT-RSIT-ELMTO-DESC.
              49  58RT-RSIT-ELMTO-DESC-LEN  PIC S9(4) COMP.
              49  58RT-RSIT-ELMTO-DESC-TEXT PIC X(240).
           10  58RT-TEXPUR-ELMTO-DESC       PIC S9(5)V COMP-3.
           10  58RT-CINDCD-PRZ-EXPUR        PIC X(2).
           10  58RT-HULT-ATULZ              PIC X(26).
           10  58RT-CTRANS-PROG-ATULZ       PIC X(8).
