      ******************************************************************
      * NOME BOOK : DCOMW10W                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE OPERACAO POR AGENCIA*
      * DATA      : 27/09/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 073 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW10W-E-CJUNC-DEPDC   = CODIGO DA AGENCIA                   *
      * DCOMW10W-E-CPRODT-SERVC  = CODIGO DO PRODUTO                   *
      * DCOMW10W-E-CSPRODT-SERVC = CODIGO DO SUBPRODUTO                *
      * DCOMW10W-E-DTINI-PESQ    = DATA INICIO DE PESQUISA             *
      * DCOMW10W-E-DTFIM-PESQ    = DATA FIM DE PESQUISA                *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW10W-INDICADOR-PAGINACAO = INDICA DIRECAO DA PAGINACAO     *
      * OBS.: PARA O PROCESSO NO TF A PAGINACAO EH INCIAL OU SEGUINTE  *
      * DCOMW10W-I-DANO-OPER-DESC    = CHAVE INICIAL DE PAGINACAO      *
      * DCOMW10W-I-NSEQ-OPER-DESC    = CHAVE INICIAL DE PAGINACAO      *
      * DCOMW10W-F-DANO-OPER-DESC    = CHAVE FINAL DE PAGINACAO        *
      * DCOMW10W-F-NSEQ-OPER-DESC    = CHAVE FINAL DE PAGINACAO        *
      *********************** DADOS DE RETORNO DA PAGINACAO ************
      * DCOMW10W-COD-RETORNO = CODIGO DE RETORNO PARA PAGINACAO        *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05 DCOMW10W-HEADER.
              10 DCOMW10W-COD-LAYOUT        PIC  X(08) VALUE 'DCOMW10W'.
              10 DCOMW10W-TAM-LAYOUT        PIC  9(05) VALUE 73.
           05 DCOMW10W-REGISTRO.
              10 DCOMW10W-BLOCO-ENTRADA.
                 15 DCOMW10W-E-CJUNC-DEPDC       PIC  9(05).
                 15 DCOMW10W-E-CPRODT-SERVC      PIC  9(03).
                 15 DCOMW10W-E-CSPRODT-SERVC     PIC  9(03).
                 15 DCOMW10W-E-DTINI-PESQ        PIC  X(10).
                 15 DCOMW10W-E-DTFIM-PESQ        PIC  X(10).
             10 DCOMW10W-BLOCO-PAGINACAO.
                 15 DCOMW10W-INDICADOR-PAGINACAO PIC  X(01).
                   88 DCOMW10W-P-INICIAL              VALUE 'I'.
                   88 DCOMW10W-P-PRIMEIRA             VALUE 'P'.
                   88 DCOMW10W-P-SEGUINTE             VALUE 'S'.
                   88 DCOMW10W-P-ANTERIOR             VALUE 'A'.
                   88 DCOMW10W-P-ULTIMA               VALUE 'U'.
                15 DCOMW10W-CHAVE-INI.
                   20 DCOMW10W-I-DANO-OPER-DESC  PIC  9(04).
                   20 DCOMW10W-I-NSEQ-OPER-DESC  PIC  9(09).
                15 DCOMW10W-CHAVE-FIM.
                   20 DCOMW10W-F-DANO-OPER-DESC  PIC  9(04).
                   20 DCOMW10W-F-NSEQ-OPER-DESC  PIC  9(09).
             10 DCOMW10W-BLOCO-RETORNO.
                15 DCOMW10W-COD-RETORNO          PIC  9(02).
