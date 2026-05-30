      *****************************************************************
      * INCLUDE PARA TABELA:          - DB2PRD.TCONVE_GRAL_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      *****************************************************************
       01  XZ-ARGUMENTOS-ENTRADA.

       05  XZ-COLUNAS-DA-TABELA.
           10 XZ-CPRODT                        PIC 9(03).
           10 XZ-CSPROD-DESC-COML              PIC 9(03).
           10 XZ-CEMPR-INC                     PIC 9(03).
           10 XZ-CDEPDC                        PIC 9(05).
           10 XZ-DIRETORIA                     PIC 9(05).
           10 XZ-GERENCIA                      PIC 9(05).
           10 XZ-SEGMENTO                      PIC 9(05).
           10 XZ-CSGL-UF                       PIC X(02).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  XZ-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-----------------------------------------------*
       05  XZ-INSTRUCAO                          PIC X(10).
      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  XZ-RETORNO.
       05  XZ-COD-RETORNO                         PIC 9(04).
       05  XZ-MENSAGEM                            PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  XZ-PROXIMO-RESTART                     PIC 9(05).
       05  XZ-QTDE-RETORNADA                      PIC 9(03).
       05  XZ-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  XZ-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TXZ.
           10 XZRT-CCONVE-GRAL-DESC              PIC 9(09).
           10 XZRT-CVRSAO-CONVE-GRAL             PIC 9(03).
           10 XZRT-ICONVE-GRAL-DESC              PIC X(40).
           10 XZRT-DINIC-VGCIA-CONVE             PIC X(10).
           10 XZRT-DVCTO-CONVE-DESC              PIC X(10).
