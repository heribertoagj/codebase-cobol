      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  28-ARGUMENTOS-ENTRADA.

       05  28-COLUNAS-DA-TABELA.
           10  28-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  28-CSIT-DESC-COML           PIC S9(3)V COMP-3.
           10  28-DINIC-EXPUR-ELMTO        PIC X(10).
           10  28-TEXPUR-ELMTO-DESC        PIC S9(5)V COMP-3.
           10  28-CINDCD-PRZ-EXPUR         PIC X(2).
           10  28-HULT-ATULZ               PIC X(26).
           10  28-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  28-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  28-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  28-RETORNO.
       05  28-COD-RETORNO                         PIC 9(04).
       05  28-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  28-PROXIMO-RESTART                     PIC 9(05).
       05  28-QTDE-RETORNADA                      PIC 9(03).
       05  28-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  28-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T28.
           10  28RT-CELMTO-DESC-COML       PIC S9(3)V COMP-3.
           10  28RT-CSIT-DESC-COML         PIC S9(3)V COMP-3.
           10  28RT-DINIC-EXPUR-ELMTO      PIC X(10).
           10  28RT-TEXPUR-ELMTO-DESC      PIC S9(5)V COMP-3.
           10  28RT-CINDCD-PRZ-EXPUR       PIC X(2).
           10  28RT-HULT-ATULZ             PIC X(26).
           10  28RT-CTRANS-PROG-ATULZ      PIC X(8).
