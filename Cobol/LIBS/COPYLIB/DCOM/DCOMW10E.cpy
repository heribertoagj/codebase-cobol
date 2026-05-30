      ******************************************************************
      * NOME BOOK : DCOMW10E                                           *
      * DESCRICAO : BOOK DE ENTRADA DA PESQUISA DE OPERACAO POR AGENCIA*
      *             UTILIZADO NO PROGRAMA DCOM1100                     *
      * DATA      : 27/09/2017                                         *
      * AUTOR     : ANALICE CHEFFER                                    *
      * EMPRESA   : BRQ - POOL DE SERVICOS                             *
      * COMPONENTE:                                                    *
      * TAMANHO   : 044 BYTES                                          *
      *********************** DADOS DE ENTRADA *************************
      * DCOMW10E-CJUNC-DEPDC     = CODIGO DA AGENCIA                   *
      * DCOMW10E-CPRODT-SERVC    = CODIGO DO PRODUTO                   *
      * DCOMW10E-CSPRODT-SERVC   = CODIGO DO SUBPRODUTO                *
      * DCOMW10E-DTINI-PESQ      = DATA INICIO DE PESQUISA             *
      * DCOMW10E-DTFIM-PESQ      = DATA FIM DE PESQUISA                *
      ******************************************************************
      *-------------------- MANUTENCOES -------------------------------*
      ******************************************************************
      * DATA       AUTOR/EMPRESA     DESCRICAO DA MANUTENCAO           *
      * 99/99/9999 AAAAAAAA/EEEEEEEE DDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDDD *
      ******************************************************************
           05  DCOMW10E-HEADER.
               10 DCOMW10E-COD-LAYOUT       PIC  X(08) VALUE 'DCOMW10E'.
               10 DCOMW10E-TAM-LAYOUT       PIC  9(05) VALUE 44.
           05  DCOMW10E-BLOCO-ENTRADA.
               10 DCOMW10E-CJUNC-DEPDC      PIC  9(05).
               10 DCOMW10E-CPRODT-SERVC     PIC  9(03).
               10 DCOMW10E-CSPRODT-SERVC    PIC  9(03).
               10 DCOMW10E-DTINI-PESQ       PIC  X(10).
               10 DCOMW10E-DTFIM-PESQ       PIC  X(10).
