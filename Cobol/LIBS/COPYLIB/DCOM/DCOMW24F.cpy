      ******************************************************************
      * NOME BOOK : DCOMW24F - BOOK DE INTERFACE COM SERVICO DCOM424F  *
      * DATA      : 05/10/2017                                         *
      * AUTOR     : GUSTAVO TRIGO                                      *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 178 BYTES                                          *
      ******************************************************************
      * ARGUMENTOS DE ENTRADA                                         *
      * OBS.: OS MODULOS DE SELECT, DELETE E UPDATE TRABALHAM SEMPRE  *
      *       COM A CHAVE PRIMARIA                                    *
      *****************************************************************
       05  24F-BLOCO-ENTRADA.
           10  24F-E-TP-PESQUISA           PIC 9(1).
           10  24F-E-CPRODT                PIC 9(3).
           10  24F-E-DINIC-PARMZ-CONTR     PIC X(10).
           10  24F-E-CTPO-COBR-TAC         PIC 9(3).
           10  24F-E-CTAC-CONTR-LIM        PIC 9(5).
           10  24F-E-CTAC-PERC-CONTR       PIC 9(5).
           10  24F-E-CTAC-MIN-CONTR        PIC 9(5).
           10  24F-E-CTAC-MAX-CONTR        PIC 9(5).
           10  24F-E-DFIM-PARMZ-CONTR      PIC X(10).
           10  24F-E-HULT-ATULZ            PIC X(26).
           10  24F-E-CFUNC-BDSCO           PIC 9(9).
           10  24F-E-CTERM                 PIC X(8).
       05  24F-BLOCO-SAIDA.
           10  24F-S-CPRODT                PIC 9(3).
           10  24F-S-DINIC-PARMZ-CONTR     PIC X(10).
           10  24F-S-CTPO-COBR-TAC         PIC 9(3).
           10  24F-S-CTAC-CONTR-LIM        PIC 9(5).
           10  24F-S-CTAC-PERC-CONTR       PIC 9(5).
           10  24F-S-CTAC-MIN-CONTR        PIC 9(5).
           10  24F-S-CTAC-MAX-CONTR        PIC 9(5).
           10  24F-S-DFIM-PARMZ-CONTR      PIC X(10).
           10  24F-S-HULT-ATULZ            PIC X(26).
           10  24F-S-CFUNC-BDSCO           PIC 9(9).
           10  24F-S-CTERM                 PIC X(8).
