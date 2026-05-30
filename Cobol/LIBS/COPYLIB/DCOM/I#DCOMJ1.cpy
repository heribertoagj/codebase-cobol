      *----------------------------------------------------------------*
      *    INC = I#DCOMJ1                                              *
      *----------------------------------------------------------------*
      *        AREA DE COMUNICACAO DO MODULO DCOM5525                  *
      *        OBTER DADOS DA OPERACAO TRANSITORIA/OFICIAL/HISTORICO   *
      *        ESPECIFICO PARA CONSULTA OPERACAO                       *
      *                                                                *
      ******************************************************************
       01  J1-ARGUMENTOS-ENTRADA.

       05  J1-COLUNAS-DA-TABELA.
           10  J1-CBCO                         PIC  9(3)V USAGE COMP-3.
           10  J1-CAG-BCRIA-I                  PIC S9(5)V USAGE COMP-3.
           10  J1-CAG-BCRIA-F                  PIC S9(5)V USAGE COMP-3.
           10  J1-CAG-OPER-OPER-I              PIC S9(5)V USAGE COMP-3.
           10  J1-CAG-OPER-OPER-F              PIC S9(5)V USAGE COMP-3.
           10  J1-CCTA-BCRIA-CLI-I             PIC S9(13)V USAGE COMP-3.
           10  J1-CCTA-BCRIA-CLI-F             PIC S9(13)V USAGE COMP-3.
           10  J1-CCONVE-CLI-DESC-I            PIC S9(9)V USAGE COMP-3.
           10  J1-CCONVE-CLI-DESC-F            PIC S9(9)V USAGE COMP-3.
           10  J1-CCONVE-GRAL-DESC-I           PIC S9(9)V USAGE COMP-3.
           10  J1-CCONVE-GRAL-DESC-F           PIC S9(9)V USAGE COMP-3.
           10  J1-CCONTR-LIM-DESC-I            PIC S9(9)V USAGE COMP-3.
           10  J1-CCONTR-LIM-DESC-F            PIC S9(9)V USAGE COMP-3.
           10  J1-CPRODT-I                     PIC S9(3)V USAGE COMP-3.
           10  J1-CPRODT-F                     PIC S9(3)V USAGE COMP-3.
           10  J1-CSPROD-DESC-COML-I           PIC S9(3)V USAGE COMP-3.
           10  J1-CSPROD-DESC-COML-F           PIC S9(3)V USAGE COMP-3.
           10  J1-DINIC-OPER-DESC-I            PIC  X(10).
           10  J1-DINIC-OPER-DESC-F            PIC  X(10).
           10  J1-CELMTO-DESC-COML             PIC S9(3)V USAGE COMP-3.
           10  J1-CCNPJ-CPF-I                  PIC S9(9)V USAGE COMP-3.
           10  J1-CCNPJ-CPF-F                  PIC S9(9)V USAGE COMP-3.
           10  J1-CFLIAL-CNPJ-I                PIC S9(5)V USAGE COMP-3.
           10  J1-CFLIAL-CNPJ-F                PIC S9(5)V USAGE COMP-3.
           10  J1-CCTRL-CNPJ-CPF-I             PIC S9(2)V USAGE COMP-3.
           10  J1-CCTRL-CNPJ-CPF-F             PIC S9(2)V USAGE COMP-3.
           10  J1-CCNPJ-CPF-SAC-I              PIC S9(9)V USAGE COMP-3.
           10  J1-CCNPJ-CPF-SAC-F              PIC S9(9)V USAGE COMP-3.
           10  J1-CFLIAL-CNPJ-SAC-I            PIC S9(5)V USAGE COMP-3.
           10  J1-CFLIAL-CNPJ-SAC-F            PIC S9(5)V USAGE COMP-3.
           10  J1-CCTRL-CNPJ-CPF-SAC-I         PIC S9(2)V USAGE COMP-3.
           10  J1-CCTRL-CNPJ-CPF-SAC-F         PIC S9(2)V USAGE COMP-3.
           10  J1-CSIT-DESC-COML-I             PIC S9(3)V USAGE COMP-3.
           10  J1-CSIT-DESC-COML-F             PIC S9(3)V USAGE COMP-3.
           10  J1-CINDCD-EVNTO-WORKFLOW-I      PIC S9(3)V USAGE COMP-3.
           10  J1-CINDCD-EVNTO-WORKFLOW-F      PIC S9(3)V USAGE COMP-3.
           10  J1-CINDCD-SIT-FINAL-EVNTO-I     PIC S9(3)V USAGE COMP-3.
           10  J1-CINDCD-SIT-FINAL-EVNTO-F     PIC S9(3)V USAGE COMP-3.
           10  J1-COD-SITUACAO-I               PIC S9(3)V USAGE COMP-3.
           10  J1-COD-SITUACAO-F               PIC S9(3)V USAGE COMP-3.
           10  J1-CTPO-BAIXA-PCELA-I           PIC  X(1).
           10  J1-CTPO-BAIXA-PCELA-F           PIC  X(1).
           10  J1-DEFETV-BAIXA-PCELA-I         PIC  X(10).
           10  J1-DEFETV-BAIXA-PCELA-F         PIC  X(10).
BSI001     10  J1-CINDCD-PSSOA-DESC            PIC  X(2).
BSI001     10  J1-NPSSOA-DESC-COML-I           PIC S9(9)V USAGE COMP-3.
BSI001     10  J1-NPSSOA-DESC-COML-F           PIC S9(9)V USAGE COMP-3.
BSI001     10  J1-CCONTR-CORP-OPER-I           PIC S9(7)V USAGE COMP-3.
BSI001     10  J1-CCONTR-CORP-OPER-F           PIC S9(7)V USAGE COMP-3.

      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-------------------------------------*
       05  J1-QTDE-A-DESPREZAR                   PIC 9(05).


      * PREENCHER O CAMPO ABAIXO, PARA FETCH
      *-----------------------------------------------*
       05  J1-INSTRUCAO                          PIC X(10).

      *****************************************************************
      * AREA DE RETORNO                                               *
      *****************************************************************
       01  J1-RETORNO.
       05  J1-COD-RETORNO                         PIC 9(04).
       05  J1-MENSAGEM                            PIC X(79).


      * SERAO PREENCHIDOS NO CASO DE FETCH
      *------------------------------------*
       05  J1-PROXIMO-RESTART                     PIC 9(05).
       05  J1-QTDE-RETORNADA                      PIC 9(03).
       05  J1-QTDE-TOTAL                          PIC 9(09).


      * AREA DE RETORNO DO SELECT (PRIMEIRA OCORRENCIA)
      * AREA DE RETORNO DO FETCH  (DIVERSAS OCORRENCIAS)
      *---------------------------------------------------------------*
       05  J1-OCORRENCIAS     OCCURS 070 TIMES INDEXED BY IND-TJ1.
           10  J1RT-DANO-OPER-DESC             PIC  9(4).
           10  J1RT-NSEQ-OPER-DESC             PIC  9(9).
      *        CAMPO IPSSOA VALORIZADO PELO MODULO DE NEGOCIO
           10  J1RT-IPSSOA-DESC-COML           PIC  X(60).
           10  J1RT-CBCO                       PIC  9(3).
           10  J1RT-CAG-BCRIA                  PIC  9(5).
           10  J1RT-CAG-BCRIA-DESC             PIC  X(40).
           10  J1RT-CCTA-BCRIA-CLI             PIC  9(13).
           10  J1RT-CBCO-OPER-OPER             PIC  9(3).
           10  J1RT-CAG-OPER-OPER              PIC  9(5).
*********> DESCRICOES SITUACAO - OPERACAO
           10  J1RT-SITUACAO-OPERACAO.
               15  J1RT-CSIT-DESC-COML         PIC  9(3).
               15  J1RT-ISIT-DESC-COML         PIC  X(30).
               15  J1RT-IRSUMO-SIT-DESC        PIC  X(15).
               15  J1RT-CSGL-SIT-DESC          PIC  X(6).
*********> DESCRICOES SITUACAO - FINAL WORKFLOW
           10  J1RT-SITUACAO-WORKFLOW.
               15  J1RT-CSIT-FNAL-DESC         PIC  9(3).
               15  J1RT-ISIT-FNAL-COML         PIC  X(30).
               15  J1RT-IRSUMO-SIT-FNAL        PIC  X(15).
               15  J1RT-CSGL-SIT-FNAL          PIC  X(6).
*********> DESCRICOES - PRODUTO
           10  J1RT-DESCRICAO-PRODUTO.
               15  J1RT-CPRODT                 PIC  9(3).
               15  J1RT-IPRODT                 PIC  X(60).
               15  J1RT-IABREV-PRODT           PIC  X(12).
               15  J1RT-CSGL-PRODT-DESC        PIC  X(06).
*********> DESCRICOES - SUBPRODUTO
           10  J1RT-DESCRICAO-SUBPRODUTO.
               15  J1RT-CSPROD-DESC-COML       PIC  9(3).
               15  J1RT-CTPO-DESC-COML         PIC  9(3).
               15  J1RT-ISPROD-DESC-COML       PIC  X(40).
               15  J1RT-IRSUMO-SPROD-DESC      PIC  X(10).
               15  J1RT-CSGL-SPROD-DESC        PIC  X(6).
               15  J1RT-VOPER-DESC-COML        PIC  9(15)V9(2).
               15  J1RT-DINIC-OPER-DESC        PIC  X(10).
               15  J1RT-DVCTO-FNAL-OPER        PIC  X(10).
*********> DESCRICOES - MEIO E OUTRAS
               15  J1RT-CMEIO-ENTRD-DESC       PIC  9(03).
               15  J1RT-IMEIO-ENTRD-DESC       PIC  X(25).
               15  J1RT-IRSUMO-MEIO-ENTRD      PIC  X(15).
               15  J1RT-CSGL-MEIO-ENTRD        PIC  X(06).
               15  J1RT-CINDCD-LIBRC-ANTCP     PIC  X(01).
               15  J1RT-PARECER-FORML          PIC  X(01).
               15  J1RT-VLIQ-OPER-DESC         PIC  9(15)V9(2).
BSI001         15  J1RT-HULT-ATULZ             PIC  X(26).
