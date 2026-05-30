      ******************************************************************
      * NOME BOOK : DCOMW1LW                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE CONTRATO POR AGENCIA*
      *             CONTA OU CNPJ/CPF E PRODUTO (OPCIONAL)             *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 072 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1LW-CJUNC-DEPDC     = CODIGO DA AGENCIA                   *
      * DCOMW1LW-CCTA-BCRIA-CLI  = NUMERO DA CONTA                     *
      * DCOMW1LW-CCNPJ-CPF       = CORPO DO CNPJ/CPF                   *
      * DCOMW1LW-CFLIAL-CNPJ     = FILIAL DO CNPJ OU ZEROS PARA CPF    *
      * DCOMW1LW-CCTRL-CNPJ-CPF  = CONTROLE DO CNPJ/CPF                *
      * DCOMW1LW-CPRODT-SERVC    = CODIGO DO PRODUTO                   *
      * DCOMW1LW-CSPRODT-SERVC   = CODIGO DO SUBPRODUTO                *
      * DCOMW1LW-DTINI-PESQ      = DATA INICIO DE PESQUISA             *
      * DCOMW1LW-DTFIM-PESQ      = DATA FIM DE PESQUISA                *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMW1LW-INDICADOR-PAGINACAO = INDICA DIRECAO DA PAGINACAO     *
      * OBS.: PARA O PROCESSO NO TF A PAGINACAO EH INCIAL OU SEGUINTE  *
      * DCOMW1LW-I-DANO-OPER-DESC    = CHAVE INICIAL DE PAGINACAO      *
      * DCOMW1LW-I-NSEQ-OPER-DESC    = CHAVE INICIAL DE PAGINACAO      *
      * DCOMW1LW-F-DANO-OPER-DESC    = CHAVE FINAL DE PAGINACAO        *
      * DCOMW1LW-F-NSEQ-OPER-DESC    = CHAVE FINAL DE PAGINACAO        *
      *********************** DADOS DE RETORNO DA PAGINACAO ************
      * DCOMW1LW-COD-RETORNO = CODIGO DE RETORNO PARA PAGINACAO        *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1LW-HEADER.
               10 DCOMW1LW-COD-LAYOUT       PIC  X(08) VALUE 'DCOMW1LW'.
               10 DCOMW1LW-TAM-LAYOUT       PIC  9(05) VALUE 72.
           05  DCOMW1LW-REGISTRO.
               10  DCOMW1LW-BLOCO-ENTRADA.
                   15  DCOMW1LW-CJUNC-DEPDC           PIC  9(05).
                   15  DCOMW1LW-CCTA-BCRIA-CLI        PIC  9(13).
                   15  DCOMW1LW-CCNPJ-CPF             PIC  9(09).
                   15  DCOMW1LW-CFLIAL-CNPJ           PIC  9(04).
                   15  DCOMW1LW-CCTRL-CNPJ-CPF        PIC  9(02).
                   15  DCOMW1LW-CPRODT-SERVC          PIC  9(03).
                   15  DCOMW1LW-CSPRODT-SERVC         PIC  9(03).
                   15  DCOMW1LW-DTINI-PESQ            PIC  X(10).
                   15  DCOMW1LW-DTFIM-PESQ            PIC  X(10).
               10  DCOMW1LW-BLOCO-PAGINACAO.
                   15  DCOMW1LW-INDICADOR-PAGINACAO   PIC  X(01).
                       88 DCOMW1LW-P-INICIAL          VALUE 'I'.
                       88 DCOMW1LW-P-PRIMEIRA         VALUE 'P'.
                       88 DCOMW1LW-P-SEGUINTE         VALUE 'S'.
                       88 DCOMW1LW-P-ANTERIOR         VALUE 'A'.
                       88 DCOMW1LW-P-ULTIMA           VALUE 'U'.
                   15  DCOMW1LW-CHAVE-INI.
                       20 DCOMW1LW-I-DANO-OPER-DESC   PIC  9(04).
                       20 DCOMW1LW-I-NSEQ-OPER-DESC   PIC  9(09).
                   15  DCOMW1LW-CHAVE-FIM.
                       20 DCOMW1LW-F-DANO-OPER-DESC   PIC  9(04).
                       20 DCOMW1LW-F-NSEQ-OPER-DESC   PIC  9(09).
               10  DCOMW1LW-BLOCO-RETORNO.
                   15 DCOMW1LW-COD-RETORNO            PIC  9(02).
