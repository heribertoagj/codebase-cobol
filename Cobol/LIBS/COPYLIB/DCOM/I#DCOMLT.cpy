      *****************************************************************
      * INCLUDE PARA TABELA: DCOMB0XP - DB2PRD.TTRASI_OPER_DESC       *
      *---------------------------------------------------------------*
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       01  XP-ARGUMENTOS-ENTRADA.

       05  XP-COLUNAS-DA-TABELA.
           10  XP-CBCO                         PIC  9(3).
           10  XP-CAG-BCRIA-I                  PIC  9(5).
           10  XP-CAG-BCRIA-F                  PIC  9(5).
           10  XP-CAG-OPER-OPER-I              PIC  9(5).
           10  XP-CAG-OPER-OPER-F              PIC  9(5).
           10  XP-CCTA-BCRIA-CLI-I             PIC  9(13).
           10  XP-CCTA-BCRIA-CLI-F             PIC  9(13).
           10  XP-CPRODT-I                     PIC  9(3).
           10  XP-CPRODT-F                     PIC  9(3).
           10  XP-CSPROD-DESC-COML-I           PIC  9(3).
           10  XP-CSPROD-DESC-COML-F           PIC  9(3).
           10  XP-DINIC-OPER-DESC-I            PIC  X(10).
           10  XP-DINIC-OPER-DESC-F            PIC  X(10).
           10  XP-CELMTO-DESC-COML             PIC  9(3).
           10  XP-CCNPJ-CPF                    PIC  9(9).
           10  XP-CFLIAL-CNPJ                  PIC  9(5).
           10  XP-CCTRL-CNPJ-CPF               PIC  9(2).
           10  XP-CSIT-DESC-COML-1             PIC  9(3).
           10  XP-CSIT-DESC-COML-2             PIC  9(3).
           10  XP-CINDCD-EVNTO-WORKFLOW-I      PIC  9(3).
           10  XP-CINDCD-EVNTO-WORKFLOW-F      PIC  9(3).
           10  XP-CINDCD-SIT-FINAL-EVNTO-I     PIC  9(3).
           10  XP-CINDCD-SIT-FINAL-EVNTO-F     PIC  9(3).

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  XP-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-----------------------------------------------*
       05  XP-INSTRUCAO                          PIC X(10).
**************  TRANSITORIA
**************     PARCIAL-01 = OPERACOES ATE EFETIVACAO
**************  TRANSITORIA
**************     PARCIAL-02 = CONSULTA(PROVISORIO) - SEM SITUACAO
**************  TRANSITORIA
**************     PARCIAL-03 = OPERACOES PARA VALIDACAO
**************  TRANSITORIA
**************     PARCIAL-04 = OPERACOES PARA FORMALIZACAO
**************                + OPERACOES PARA CANCELAMENTO FORMALIZACAO
**************  TRANSITORIA
**************     PARCIAL-05 = OPERACAOES PARA LIBERACAO
**************  TRANSITORIA
**************     PARCIAL-06 = OPERACOES PARA ALTERACAO VALIDACAO
**************  TRANSITORIA
**************     PARCIAL-07 = OPERACAOES PARA CANCELAMENTO LIBERACAO
**************
************** (PAULO FERRARINI - JULHO 2007)
**************
      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  XP-RETORNO.
       05  XP-COD-RETORNO                         PIC 9(04).
       05  XP-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  XP-PROXIMO-RESTART                     PIC 9(05).
       05  XP-QTDE-RETORNADA                      PIC 9(03).
       05  XP-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  XP-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TXP.
           10  XPRT-DANO-OPER-DESC             PIC  9(4).
           10  XPRT-NSEQ-OPER-DESC             PIC  9(9).
      *        CAMPO IPSSOA VALORIZADO PELO MODULO DE NEGOCIO
           10  XPRT-IPSSOA-DESC-COML           PIC  X(60).
           10  XPRT-CBCO                       PIC  9(3).
           10  XPRT-CAG-BCRIA                  PIC  9(5).
           10  XPRT-CAG-BCRIA-DESC             PIC  X(40).
           10  XPRT-CCTA-BCRIA-CLI             PIC  9(13).
           10  XPRT-CBCO-OPER-OPER             PIC  9(3).
           10  XPRT-CAG-OPER-OPER              PIC  9(5).
           10  XPRT-CCNPJ-CPF                  PIC  9(9).
           10  XPRT-CFLIAL-CNPJ                PIC  9(5).
           10  XPRT-CCTRL-CNPJ-CPF             PIC  9(2).
*********> DESCRICOES SITUACAO - OPERACAO
           10  XPRT-SITUACAO-OPERACAO.
               15  XPRT-CSIT-DESC-COML         PIC  9(3).
               15  XPRT-ISIT-DESC-COML         PIC  X(30).
               15  XPRT-IRSUMO-SIT-DESC        PIC  X(15).
               15  XPRT-CSGL-SIT-DESC          PIC  X(6).
*********> DESCRICOES SITUACAO - FINAL WORKFLOW
           10  XPRT-SITUACAO-WORKFLOW.
               15  XPRT-CSIT-FNAL-DESC         PIC  9(3).
               15  XPRT-ISIT-FNAL-COML         PIC  X(30).
               15  XPRT-IRSUMO-SIT-FNAL        PIC  X(15).
               15  XPRT-CSGL-SIT-FNAL          PIC  X(6).
*********> DESCRICOES - PRODUTO
           10  XPRT-DESCRICAO-PRODUTO.
               15  XPRT-CPRODT                 PIC  9(3).
               15  XPRT-IPRODT                 PIC  X(60).
               15  XPRT-IABREV-PRODT           PIC  X(12).
               15  XPRT-CSGL-PRODT-DESC        PIC  X(06).
*********> DESCRICOES - SUBPRODUTO
           10  XPRT-DESCRICAO-SUBPRODUTO.
               15  XPRT-CSPROD-DESC-COML       PIC  9(3).
               15  XPRT-CTPO-DESC-COML         PIC  9(3).
               15  XPRT-ISPROD-DESC-COML       PIC  X(40).
               15  XPRT-IRSUMO-SPROD-DESC      PIC  X(10).
               15  XPRT-CSGL-SPROD-DESC        PIC  X(6).
               15  XPRT-VOPER-DESC-COML        PIC  9(15)V9(2).
               15  XPRT-DINIC-OPER-DESC        PIC  X(10).
               15  XPRT-DVCTO-FNAL-OPER        PIC  X(10).
*********> DESCRICOES - MEIO E OUTRAS
NEW            15  XPRT-CMEIO-ENTRD-DESC       PIC  9(03).
NEW            15  XPRT-IMEIO-ENTRD-DESC       PIC  X(25).
NEW            15  XPRT-IRSUMO-MEIO-ENTRD      PIC  X(15).
NEW            15  XPRT-CSGL-MEIO-ENTRD        PIC  X(06).
NEW            15  XPRT-CINDCD-LIBRC-ANTCP     PIC  X(01).
NEW            15  XPRT-PARECER-FORML          PIC  X(01).
NEW            15  XPRT-VLIQ-OPER-DESC         PIC  9(15)V9(2).
