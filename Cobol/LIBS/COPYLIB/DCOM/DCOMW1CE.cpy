      ******************************************************************
      * NOME BOOK : DCOMW1CE                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE OPERACAO POR AGENCIA*
      * DATA      : 16/10/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 072 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW1CE-CJUNC-DEPDC     = CODIGO DA AGENCIA                   *
      * DCOMW1CE-CCNPJ-CPF       = CODIGO DA CNPJ OU CPF               *
      * DCOMW1CE-CPRODT-SERVC    = CODIGO DO PRODUTO                   *
      * DCOMW1CE-CSPRODT-SERVC   = CODIGO DO SUBPRODUTO                *
      * DCOMW1CE-DTINI-PESQ      = DATA INICIO DE PESQUISA             *
      * DCOMW1CE-DTFIM-PESQ      = DATA FIM DE PESQUISA                *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW1CE-HEADER.
               10 DCOMW1CE-COD-LAYOUT       PIC  X(08) VALUE 'DCOMW1CE'.
               10 DCOMW1CE-TAM-LAYOUT       PIC  9(05) VALUE 72.
           05  DCOMW1CE-BLOCO-ENTRADA.
               10 DCOMW1CE-CJUNC-DEPDC      PIC  9(05).
               10 DCOMW1CE-CCNPJ-CPF        PIC  9(15).
               10 DCOMW1CE-CCONTA           PIC  9(13).
               10 DCOMW1CE-CPRODT-SERVC     PIC  9(03).
               10 DCOMW1CE-CSPRODT-SERVC    PIC  9(03).
               10 DCOMW1CE-DTINI-PESQ       PIC  X(10).
               10 DCOMW1CE-DTFIM-PESQ       PIC  X(10).
