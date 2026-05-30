      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0XQ - DB2PRD.TCONTR_LIM_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  XQ-ARGUMENTOS-ENTRADA.

       05  XQ-COLUNAS-DA-TABELA.
           10  XQ-CBCO                         PIC  9(3).
           10  XQ-CAG-BCRIA-I                  PIC  9(5).
           10  XQ-CAG-BCRIA-F                  PIC  9(5).
           10  XQ-CCTA-BCRIA-CLI-I             PIC  9(13).
           10  XQ-CCTA-BCRIA-CLI-F             PIC  9(13).
           10  XQ-DINIC-OPER-DESC-I            PIC  X(10).
           10  XQ-DINIC-OPER-DESC-F            PIC  X(10).
           10  XQ-CCNPJ-CPF                    PIC  9(9).
           10  XQ-CFLIAL-CNPJ                  PIC  9(5).
           10  XQ-CCTRL-CNPJ-CPF               PIC  9(2).
           10  XQ-CSIT-DESC-COML-1             PIC  9(3).
           10  XQ-CSIT-DESC-COML-2             PIC  9(3).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  XQ-QTDE-A-DESPREZAR                   PIC 9(05).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-----------------------------------------------*
       05  XQ-INSTRUCAO                          PIC X(10).
**************     PARCIAL-01 = ESTORNO DE CONTRATO LIMITE
**************     PARCIAL-02 = MOVIMENTO DE CONTRATO LIMITE ESTORNADOS

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  XQ-RETORNO.
       05  XQ-COD-RETORNO                         PIC 9(04).
       05  XQ-MENSAGEM                            PIC X(79).

      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  XQ-PROXIMO-RESTART                     PIC 9(05).
       05  XQ-QTDE-RETORNADA                      PIC 9(03).
       05  XQ-QTDE-TOTAL                          PIC 9(09).

      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  XQ-OCORRENCIAS     OCCURS 010 TIMES INDEXED BY IND-TXQ.
           10  XQRT-CCONTR-LIM-DESC            PIC  9(09).
           10  XQRT-CVRSAO-CONTR-LIM           PIC  9(03).
           10  XQRT-IPSSOA-DESC-COML           PIC  X(60).
           10  XQRT-CSIT-DESC-COML             PIC  9(03).
           10  XQRT-ISIT-DESC-COML             PIC  X(30).
           10  XQRT-IRSUMO-SIT-DESC            PIC  X(15).
           10  XQRT-CSGL-SIT-DESC              PIC  X(06).
           10  XQRT-VCONTR-LIM-DESC            PIC  9(15)V9(2).
           10  XQRT-DINIC-VGCIA-CONTR          PIC  X(10).
           10  XQRT-DVCTO-CONTR-LIM            PIC  X(10).
