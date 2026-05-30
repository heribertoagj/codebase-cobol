      ******************************************************************
      * NOME BOOK : DCOMW1AI                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE CONTRATO POR AGENCIA*
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 20232 BYTES                                        *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1AI-E-CJUNC-DEPDC   = CODIGO DA AGENCIA                   *
      * DCOMW1AI-E-CPRODT-SERVC  = CODIGO DO PRODUTO                   *
      * DCOMW1AI-E-CSPRODT-SERVC = CODIGO DO SUBPRODUTO                *
      * DCOMW1AI-E-DTINI-PESQ    = DATA INICIO PARA PESQUISA           *
      * DCOMW1AI-E-DTFIM-PESQ    = DATA FIM PARA PESQUESA              *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW1AI-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: ESTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      * DCOMW1AI-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMW1AI-I-CCONTR-LIM-DESC  = NUMERO DO CONTRATO INICIAL       *
      * DCOMW1AI-I-CVRSAO-CONTR-LIM = VERSAO DO CONTRATO INICIAL       *
      * DCOMW1AI-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMW1AI-F-CCONTR-LIM-DESC  = NUMERO DO CONTRATO FINAL         *
      * DCOMW1AI-F-CVRSAO-CONTR-LIM = VERSAO DO CONTRATO FINAL         *
      *********************** DADOS DE SAIDA ***************************
      * DCOMW1AI-S-NUM-CONSULTAS   = NUMERO DE CONTRATOS PESQUISADOS   *
      * DCOMW1AI-S-CCONTR-LIM-DESC = NUMERO DO CONTRATO DE LIMITE      *
      * DCOMW1AI-S-CJUNC-DEPDC     = CODIGO DA AGENCIA                 *
      * DCOMW1AI-S-CCTA-CLIE       = NUMERO DA CONTA                   *
      * DCOMW1AI-S-DIG-CCTA-CLIE   = DIGITO DA CONTA                   *
      * DCOMW1AI-S-IPSSOA-CLIE     = NOME DO CLIENTE                   *
      * DCOMW1AI-S-DVCTO-CONTR-LIM = DIGO DO PRODUTO                   *
      * DCOMW1AI-S-VCONTR-LIM-DESC = VALOR DO CONTRATO DE LIMITE       *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1AI-HEADER.
               10  DCOMW1AI-COD-LAYOUT      PIC  X(08) VALUE 'DCOMW1AI'.
               10  DCOMW1AI-TAM-LAYOUT      PIC  9(05) VALUE 20232.
           05  DCOMW1AI-REGISTRO.
               10  DCOMW1AI-BLOCO-ENTRADA.
                   15  DCOMW1AI-E-CJUNC-DEPDC         PIC  9(05).
                   15  DCOMW1AI-E-CPRODT-SERVC        PIC  9(03).
                   15  DCOMW1AI-E-CSPRODT-SERVC       PIC  9(03).
                   15  DCOMW1AI-E-DTINI-PESQ          PIC  X(10).
                   15  DCOMW1AI-E-DTFIM-PESQ          PIC  X(10).
               10  DCOMW1AI-BLOCO-PAGINACAO.
                   15  DCOMW1AI-INDICADOR-PAGINACAO   PIC  X(01).
                       88  DCOMW1AI-P-INICIAL              VALUE 'I'.
                       88  DCOMW1AI-P-PRIMEIRA             VALUE 'P'.
                       88  DCOMW1AI-P-SEGUINTE             VALUE 'S'.
                       88  DCOMW1AI-P-ANTERIOR             VALUE 'A'.
                       88  DCOMW1AI-P-ULTIMA               VALUE 'U'.
                   15  DCOMW1AI-CHAVE-INI.
                       20  DCOMW1AI-I-CCONTR-LIM-DESC  PIC  9(09).
                       20  DCOMW1AI-I-CVRSAO-CONTR-LIM PIC  9(03).
                   15  DCOMW1AI-CHAVE-FIM.
                       20  DCOMW1AI-F-CCONTR-LIM-DESC  PIC  9(09).
                       20  DCOMW1AI-F-CVRSAO-CONTR-LIM PIC  9(03).
               10  DCOMW1AI-BLOCO-SAIDA.
                   15  DCOMW1AI-S-NUM-CONSULTAS       PIC  9(03).
                   15  DCOMW1AI-S-DADOS-SAIDA         OCCURS 210 TIMES.
                       20  DCOMW1AI-S-CCONTR-LIM-DESC PIC  9(09).
                       20  DCOMW1AI-S-CJUNC-DEPDC     PIC  9(05).
                       20  DCOMW1AI-S-CCTA-CLIE       PIC  9(13).
                       20  DCOMW1AI-S-DIG-CCTA-CLIE   PIC  X(02).
                       20  DCOMW1AI-S-IPSSOA-CLIE     PIC  X(40).
                       20  DCOMW1AI-S-DVCTO-CONTR-LIM PIC  X(10).
                       20  DCOMW1AI-S-VCONTR-LIM-DESC PIC  9(15)V99.
