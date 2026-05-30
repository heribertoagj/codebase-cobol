      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  32-ARGUMENTOS-ENTRADA.

       05  32-COLUNAS-DA-TABELA.
           10  32-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  32-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  32-HULT-ATULZ               PIC X(26).
           10  32-CINDCD-MANUT-REG         PIC X(1).
           10  32-CCHAVE-ELMTO-DESC        PIC X(45).
           10  32-CTRANS-PROG-ATULZ        PIC X(8).
           10  32-CINDCD-MODLD-PROCM       PIC X(1).
           10  32-CEMPR-INC                PIC S9(5)V COMP-3.
           10  32-CDEPDC                   PIC S9(5)V COMP-3.
           10  32-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  32-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  32-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  32-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  32-RETORNO.
       05  32-COD-RETORNO                         PIC 9(04).
       05  32-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  32-PROXIMO-RESTART                     PIC 9(05).
       05  32-QTDE-RETORNADA                      PIC 9(03).
       05  32-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  32-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T32.
           10  32RT-CELMTO-DESC-COML       PIC S9(3)V COMP-3.
           10  32RT-CEVNTO-DESC-COML       PIC S9(3)V COMP-3.
           10  32RT-HULT-ATULZ             PIC X(26).
           10  32RT-CINDCD-MANUT-REG       PIC X(1).
           10  32RT-CCHAVE-ELMTO-DESC      PIC X(45).
           10  32RT-CTRANS-PROG-ATULZ      PIC X(8).
           10  32RT-CINDCD-MODLD-PROCM     PIC X(1).
           10  32RT-CEMPR-INC              PIC S9(5)V COMP-3.
           10  32RT-CDEPDC                 PIC S9(5)V COMP-3.
           10  32RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  32RT-CTERM                  PIC X(8).
