      ******************************************************************
      * NOME BOOK : DCOMW1AW                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE CONTRATO POR AGENCIA*
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 073 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1AW-E-CJUNC-DEPDC   = CODIGO DA AGENCIA                   *
      * DCOMW1AW-E-CPRODT-SERVC  = CODIGO DO PRODUTO                   *
      * DCOMW1AW-E-CSPRODT-SERVC = CODIGO DO SUBPRODUTO                *
      * DCOMW1AW-E-DTINI-PESQ    = DATA INICIO DE PESQUISA             *
      * DCOMW1AW-E-DTFIM-PESQ    = DATA FIM DE PESQUISA                *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW1AW-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: ESTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      * DCOMW1AW-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMW1AW-I-CCONTR-LIM-DESC  = NUMERO DO CONTRATO INICIAL       *
      * DCOMW1AW-I-CVRSAO-CONTR-LIM = VERSAO DO CONTRATO INICIAL       *
      * DCOMW1AW-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMW1AW-F-CCONTR-LIM-DESC  = NUMERO DO CONTRATO FINAL         *
      * DCOMW1AW-F-CVRSAO-CONTR-LIM = VERSAO DO CONTRATO FINAL         *
      *********************** DADOS DE SAIDA ***************************
      * DCOMW1AW-COD-RETORNO = CODIGO DE RETORNO PARA PAGINACAO        *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05 DCOMW1AW-HEADER.
              10 DCOMW1AW-COD-LAYOUT        PIC  X(08) VALUE 'DCOMW1AW'.
              10 DCOMW1AW-TAM-LAYOUT        PIC  9(05) VALUE 73.
           05 DCOMW1AW-REGISTRO.
              10 DCOMW1AW-BLOCO-ENTRADA.
                 15 DCOMW1AW-E-CJUNC-DEPDC         PIC  9(05).
                 15 DCOMW1AW-E-CPRODT-SERVC        PIC  9(03).
                 15 DCOMW1AW-E-CSPRODT-SERVC       PIC  9(03).
                 15 DCOMW1AW-E-DTINI-PESQ          PIC  X(10).
                 15 DCOMW1AW-E-DTFIM-PESQ          PIC  X(10).
             10 DCOMW1AW-BLOCO-PAGINACAO.
                 15 DCOMW1AW-INDICADOR-PAGINACAO   PIC  X(01).
                   88 DCOMW1AW-P-INICIAL              VALUE 'I'.
                   88 DCOMW1AW-P-PRIMEIRA             VALUE 'P'.
                   88 DCOMW1AW-P-SEGUINTE             VALUE 'S'.
                   88 DCOMW1AW-P-ANTERIOR             VALUE 'A'.
                   88 DCOMW1AW-P-ULTIMA               VALUE 'U'.
               15  DCOMW1AW-CHAVE-INI.
                   20  DCOMW1AW-I-CCONTR-LIM-DESC  PIC  9(09).
                   20  DCOMW1AW-I-CVRSAO-CONTR-LIM PIC  9(03).
               15  DCOMW1AW-CHAVE-FIM.
                   20  DCOMW1AW-F-CCONTR-LIM-DESC  PIC  9(09).
                   20  DCOMW1AW-F-CVRSAO-CONTR-LIM PIC  9(03).
             10 DCOMW1AW-BLOCO-RETORNO.
                15 DCOMW1AW-COD-RETORNO            PIC  9(02).
