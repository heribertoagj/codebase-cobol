      ******************************************************************
      * NOME BOOK : DCOMW82I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 31/10/2017                                         *
      * AUTOR     : SHERLY GUEDES                                      *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 1097 BYTES                                         *
      ******************************************************************
       05 DCOMW82I-HEADER.
         10 DCOMW82I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW82I'.
         10 DCOMW82I-TAM-LAYOUT        PIC 9(05)   VALUE 1097.
       05 DCOMW82I-REGISTRO.
         10 DCOMW82I-BLOCO-ENTRADA.
           15 DCOMW82I-E-NUM-OPER                       PIC 9(13).
         10 DCOMW82I-BLOCO-SAIDA.
           15 DCOMW82I-S-CAG-BCRIA                       PIC 9(5).
           15 DCOMW82I-S-DIGITO-AGENCIA                  PIC X(2).
           15 DCOMW82I-S-CCTA-BCRIA-CLI                  PIC 9(13).
           15 DCOMW82I-S-DIGITO-CONTA                    PIC X(2).
           15 DCOMW82I-S-CCNPJ-CPF                       PIC 9(9).
           15 DCOMW82I-S-CFLIAL-CNPJ                     PIC 9(4).
           15 DCOMW82I-S-CCTRL-CNPJ-CPF                  PIC 9(2).
           15 DCOMW82I-S-OPERACAO                        PIC 9(13).
           15 DCOMW82I-S-DINIC-OPER-DESC                 PIC X(10).
           15 DCOMW82I-S-CCART-LIM                       PIC X(5).
           15 DCOMW82I-S-VOPER-DESC-COML                 PIC 9(15)V9(2).
           15 DCOMW82I-S-VIOF-OPER                       PIC 9(15)V9(2).
           15 DCOMW82I-S-VTAC-OPER                       PIC 9(15)V9(2).
           15 DCOMW82I-S-IPSSOA-DESC-COML                PIC X(40).
           15 DCOMW82I-S-DATA-PRIM-VCTO                  PIC X(10).
           15 DCOMW82I-S-DVCTO-FNAL-OPER                 PIC X(10).
           15 DCOMW82I-S-PTX-JURO-MES                    PIC 9(07)V9(6).
           15 DCOMW82I-S-PTX-JURO-ANO                    PIC 9(07)V9(6).
           15 DCOMW82I-S-QTITLO-CHEQ-PCELA               PIC 9(9).
           15 DCOMW82I-S-VTRIBUTOS                       PIC 9(15)V9(2).
           15 DCOMW82I-S-PTRIBUTOS                       PIC 9(3)V9(2).
           15 DCOMW82I-S-VSEGUROS                        PIC 9(15)V9(2).
           15 DCOMW82I-S-PSEGUROS                        PIC 9(3)V9(2).
           15 DCOMW82I-S-VTARIFA-CET                     PIC 9(15)V9(2).
           15 DCOMW82I-S-PTARIFA-CET                     PIC 9(3)V9(2).
           15 DCOMW82I-S-VPGTOSERV                       PIC 9(15)V9(2).
           15 DCOMW82I-S-PPGTOSERV                       PIC 9(3)V9(2).
           15 DCOMW82I-S-VREGISTROS                      PIC 9(15)V9(2).
           15 DCOMW82I-S-PREGISTROS                      PIC 9(3)V9(2).
           15 DCOMW82I-S-VCET-TOTAL                      PIC 9(15)V9(2).
           15 DCOMW82I-S-PCET-TOTAL                      PIC 9(3)V9(2).
           15 DCOMW82I-S-VCET-MES                        PIC 9(8)V9(7).
           15 DCOMW82I-S-VCET-ANO                        PIC 9(8)V9(7).
           15 DCOMW82I-S-VTOT-LIQ                        PIC 9(13)V9(2).
           15 DCOMW82I-S-VTOT-LIB                        PIC 9(15)V9(2).
           15 DCOMW82I-S-TOT-LIB                         PIC 9(3)V9(2).
           15 DCOMW82I-S-NOCOR                           PIC 9(3).
           15 DCOMW82I-S-AVAL-TESTEM               OCCURS 04 TIMES.
              20 DCOMW82I-S-NOME-AVAL                    PIC X(40).
              20 DCOMW82I-S-CNPJ-AVAL                    PIC 9(09).
              20 DCOMW82I-S-FILIAL-AVAL                  PIC 9(04).
              20 DCOMW82I-S-CTRL-AVAL                    PIC 9(02).
              20 DCOMW82I-S-NOME-CONJUGE                 PIC X(40).
              20 DCOMW82I-S-CNPJ-CNJG                    PIC 9(09).
              20 DCOMW82I-S-FILIAL-CNJG                  PIC 9(04).
              20 DCOMW82I-S-CTRL-CNJG                    PIC 9(02).
              20 DCOMW82I-S-NOME-TEST                    PIC X(40).
              20 DCOMW82I-S-CNPJ-TEST                    PIC 9(09).
              20 DCOMW82I-S-FILIAL-TEST                  PIC 9(04).
              20 DCOMW82I-S-CTRL-TEST                    PIC 9(02).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
