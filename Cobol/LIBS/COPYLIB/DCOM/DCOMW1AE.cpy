      ******************************************************************
      * NOME BOOK : DCOMW1AE                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE CONTRATO POR AGENCIA*
      * DATA      : 18/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 044 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1AE-CJUNC-DEPDC     = CODIGO DA AGENCIA                   *
      * DCOMW1AE-CPRODT-SERVC    = CODIGO DO PRODUTO                   *
      * DCOMW1AE-CSPRODT-SERVC   = CODIGO DO SUBPRODUTO                *
      * DCOMW1AE-DTINI-PESQ      = DATA INICIO DE PESQUISA             *
      * DCOMW1AE-DTFIM-PESQ      = DATA FIM DE PESQUISA                *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1AE-HEADER.
               10 DCOMW1AE-COD-LAYOUT       PIC  X(08) VALUE 'DCOMW1AE'.
               10 DCOMW1AE-TAM-LAYOUT       PIC  9(05) VALUE 44.
           05  DCOMW1AE-BLOCO-ENTRADA.
               10 DCOMW1AE-CJUNC-DEPDC      PIC  9(05).
               10 DCOMW1AE-CPRODT-SERVC     PIC  9(03).
               10 DCOMW1AE-CSPRODT-SERVC    PIC  9(03).
               10 DCOMW1AE-DTINI-PESQ       PIC  X(10).
               10 DCOMW1AE-DTFIM-PESQ       PIC  X(10).
