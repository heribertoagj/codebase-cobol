      ******************************************************************
      * NOME BOOK : DCOMW1LE                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE CONTRATO POR AGENCIA*
      *             CONTA OU CNPJ/CPF E PRODUTO (OPCIONAL)             *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 072 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1LE-CJUNC-DEPDC     = CODIGO DA AGENCIA                   *
      * DCOMW1LE-CCTA-BCRIA-CLI  = NUMERO DA CONTA                     *
      * DCOMW1LE-CCNPJ-CPF       = CORPO DO CNPJ/CPF                   *
      * DCOMW1LE-CFLIAL-CNPJ     = FILIAL DO CNPJ OU ZEROS PARA CPF    *
      * DCOMW1LE-CCTRL-CNPJ-CPF  = CONTROLE DO CNPJ/CPF                *
      * DCOMW1LE-CPRODT-SERVC    = CODIGO DO PRODUTO                   *
      * DCOMW1LE-CSPRODT-SERVC   = CODIGO DO SUBPRODUTO                *
      * DCOMW1LE-DTINI-PESQ      = DATA INICIO DE PESQUISA             *
      * DCOMW1LE-DTFIM-PESQ      = DATA FIM DE PESQUISA                *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1LE-HEADER.
               10 DCOMW1LE-COD-LAYOUT       PIC  X(08) VALUE 'DCOMW1LE'.
               10 DCOMW1LE-TAM-LAYOUT       PIC  9(05) VALUE 72.
           05  DCOMW1LE-BLOCO-ENTRADA.
               10 DCOMW1LE-CJUNC-DEPDC      PIC  9(05).
               10 DCOMW1LE-CCTA-BCRIA-CLI   PIC  9(13).
               10 DCOMW1LE-CCNPJ-CPF        PIC  9(09).
               10 DCOMW1LE-CFLIAL-CNPJ      PIC  9(04).
               10 DCOMW1LE-CCTRL-CNPJ-CPF   PIC  9(02).
               10 DCOMW1LE-CPRODT-SERVC     PIC  9(03).
               10 DCOMW1LE-CSPRODT-SERVC    PIC  9(03).
               10 DCOMW1LE-DTINI-PESQ       PIC  X(10).
               10 DCOMW1LE-DTFIM-PESQ       PIC  X(10).
