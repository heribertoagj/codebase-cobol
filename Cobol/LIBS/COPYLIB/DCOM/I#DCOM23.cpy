      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  23-ARGUMENTOS-ENTRADA.

       05  23-COLUNAS-DA-TABELA.
           10  23-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  23-IELMTO-DESC-COML         PIC X(30).
           10  23-HULT-ATULZ               PIC X(26).
           10  23-CTRANS-PROG-ATULZ        PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  23-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  23-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  23-RETORNO.
       05  23-COD-RETORNO                         PIC 9(04).
       05  23-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  23-PROXIMO-RESTART                     PIC 9(05).
       05  23-QTDE-RETORNADA                      PIC 9(03).
       05  23-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  23-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T23.
           10  23RT-CELMTO-DESC-COML       PIC S9(3)V COMP-3.
           10  23RT-IELMTO-DESC-COML       PIC X(30).
           10  23RT-HULT-ATULZ             PIC X(26).
           10  23RT-CTRANS-PROG-ATULZ      PIC X(8).
