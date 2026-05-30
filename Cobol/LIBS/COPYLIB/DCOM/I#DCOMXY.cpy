      *****************************************************************
      * INCLUDE PARA TABELA:          - DB2PRD.TLOG_COPLT_DESC        *
      *                      UNION    - DB2PRD.TLOG_RSUMO_DESC        *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      *****************************************************************
       01  XY-ARGUMENTOS-ENTRADA.

       05  XY-COLUNAS-DA-TABELA.
           10 XY-CELMTO-DESC-COML              PIC 9(03).
           10 XY-CEVNTO-DESC-COML              PIC 9(03).
           10 XY-HULT-ATULZ                    PIC X(26).
           10 XY-CINDCD-MANUT-REG              PIC X(01).
           10 XY-CCHAVE-ELMTO-DESC             PIC X(45).
           10 XY-CTRANS-PROG-ATULZ             PIC X(08).
           10 XY-CINDCD-MODLD-PROCM            PIC X(01).
           10 XY-DT-INI-PESQ                   PIC X(10).
           10 XY-DT-FIM-PESQ                   PIC X(10).
           10 XY-CPRODT                        PIC 9(03).
           10 XY-CSPROD-DESC-COML              PIC 9(03).
           10 XY-CEMPR-INC                     PIC 9(05).
           10 XY-CDEPDC                        PIC 9(05).
           10 XY-CFUNC-BDSCO                   PIC 9(09).
           10 XY-CTERM                         PIC X(08).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  XY-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-----------------------------------------------*
       05  XY-INSTRUCAO                          PIC X(10).
      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  XY-RETORNO.
       05  XY-COD-RETORNO                         PIC 9(04).
       05  XY-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  XY-PROXIMO-RESTART                     PIC 9(05).
       05  XY-QTDE-RETORNADA                      PIC 9(03).
       05  XY-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  XY-OCORRENCIAS     OCCURS 030 TIMES INDEXED BY IND-TXY.
           10 XYRT-CELMTO-DESC-COML              PIC 9(03).
           10 XYRT-CEVNTO-DESC-COML              PIC 9(03).
           10 XYRT-HULT-ATULZ                    PIC X(26).
           10 XYRT-CINDCD-MANUT-REG              PIC X(01).
           10 XYRT-CCHAVE-ELMTO-DESC             PIC X(45).
           10 XYRT-CTRANS-PROG-ATULZ             PIC X(08).
           10 XYRT-CINDCD-MODLD-PROCM            PIC X(01).
           10 XYRT-CEMPR-INC                     PIC 9(05).
           10 XYRT-CDEPDC                        PIC 9(05).
           10 XYRT-CFUNC-BDSCO                   PIC 9(09).
           10 XYRT-CTERM                         PIC X(08).
