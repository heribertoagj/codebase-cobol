      *****************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  31-ARGUMENTOS-ENTRADA.

       05  31-COLUNAS-DA-TABELA.
           10  31-CELMTO-DESC-COML         PIC S9(3)V COMP-3.
           10  31-CEVNTO-DESC-COML         PIC S9(3)V COMP-3.
           10  31-HULT-ATULZ               PIC X(26).
           10  31-CINDCD-MANUT-REG         PIC X(1).
           10  31-CCHAVE-ELMTO-DESC        PIC X(45).
           10  31-WREG-LOG-ANTES.
              49  31-WREG-LOG-ANTES-LEN    PIC S9(4) COMP.
              49  31-WREG-LOG-ANTES-TEXT   PIC X(1500).
           10  31-WREG-LOG-APOS.
              49  31-WREG-LOG-APOS-LEN     PIC S9(4) COMP.
              49  31-WREG-LOG-APOS-TEXT    PIC X(1500).
           10  31-CTRANS-PROG-ATULZ        PIC X(8).
           10  31-CINDCD-MODLD-PROCM       PIC X(1).
           10  31-CEMPR-INC                PIC S9(5)V COMP-3.
           10  31-CDEPDC                   PIC S9(5)V COMP-3.
           10  31-CFUNC-BDSCO              PIC S9(9)V COMP-3.
           10  31-CTERM                    PIC X(8).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  31-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH E UPDATE
      *-----------------------------------------------*
       05  31-INSTRUCAO                          PIC X(10).


      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  31-RETORNO.
       05  31-COD-RETORNO                         PIC 9(04).
       05  31-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  31-PROXIMO-RESTART                     PIC 9(05).
       05  31-QTDE-RETORNADA                      PIC 9(03).
       05  31-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  31-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-T31.
           10  31RT-CELMTO-DESC-COML       PIC S9(3)V COMP-3.
           10  31RT-CEVNTO-DESC-COML       PIC S9(3)V COMP-3.
           10  31RT-HULT-ATULZ             PIC X(26).
           10  31RT-CINDCD-MANUT-REG       PIC X(1).
           10  31RT-CCHAVE-ELMTO-DESC      PIC X(45).
           10  31RT-WREG-LOG-ANTES.
              49  31RT-WREG-LOG-ANTES-LEN  PIC S9(4) COMP.
              49  31RT-WREG-LOG-ANTES-TEXT PIC X(1500).
           10  31RT-WREG-LOG-APOS.
              49  31RT-WREG-LOG-APOS-LEN   PIC S9(4) COMP.
              49  31RT-WREG-LOG-APOS-TEXT  PIC X(1500).
           10  31RT-CTRANS-PROG-ATULZ      PIC X(8).
           10  31RT-CINDCD-MODLD-PROCM     PIC X(1).
           10  31RT-CEMPR-INC              PIC S9(5)V COMP-3.
           10  31RT-CDEPDC                 PIC S9(5)V COMP-3.
           10  31RT-CFUNC-BDSCO            PIC S9(9)V COMP-3.
           10  31RT-CTERM                  PIC X(8).
