      ******************************************************************
      * NOME BOOK : DCOMW84I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 31/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 1054 BYTES                                         *
      ******************************************************************
       05 DCOMW84I-HEADER.
         10 DCOMW84I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW84I'.
         10 DCOMW84I-TAM-LAYOUT        PIC 9(05)   VALUE 1054.
       05 DCOMW84I-REGISTRO.
         10 DCOMW84I-BLOCO-ENTRADA.
           15 DCOMW84I-E-NUM-OPER                       PIC 9(13).
         10 DCOMW84I-BLOCO-SAIDA.
           15 DCOMW84I-S-CAG-BCRIA                       PIC 9(5).
           15 DCOMW84I-S-DIGITO-AGENCIA                  PIC X(2).
           15 DCOMW84I-S-CCTA-BCRIA-CLI                  PIC 9(13).
           15 DCOMW84I-S-DIGITO-CONTA                    PIC X(2).
           15 DCOMW84I-S-CCNPJ-CPF                       PIC 9(9).
           15 DCOMW84I-S-CFLIAL-CNPJ                     PIC 9(4).
           15 DCOMW84I-S-CCTRL-CNPJ-CPF                  PIC 9(2).
           15 DCOMW84I-S-OPERACAO                        PIC 9(13).
           15 DCOMW84I-S-DINIC-OPER-DESC                 PIC X(10).
           15 DCOMW84I-S-VOPER-DESC-COML                 PIC 9(15)V9(2).
           15 DCOMW84I-S-IPSSOA-DESC-COML                PIC X(40).
           15 DCOMW84I-S-DATA-PRIM-VCTO                  PIC X(10).
           15 DCOMW84I-S-DVCTO-FNAL-OPER                 PIC X(10).
           15 DCOMW84I-S-PTX-JURO-MES                    PIC 9(07)V9(6).
           15 DCOMW84I-S-PTX-JURO-ANO                    PIC 9(07)V9(6).
           15 DCOMW84I-S-VIOF-OPER                       PIC 9(13)V9(2).
           15 DCOMW84I-S-VTAC-OPER                       PIC 9(13)V9(2).
           15 DCOMW84I-S-QTITLO-CHEQ                     PIC 9(09).
           15 DCOMW84I-S-VAL-OPER-EXT                    PIC X(200).
           15 DCOMW84I-S-VTRIBUTOS                       PIC 9(15)V9(2).
           15 DCOMW84I-S-PTRIBUTOS                       PIC 9(3)V9(2).
           15 DCOMW84I-S-VSEGUROS                        PIC 9(15)V9(2).
           15 DCOMW84I-S-PSEGUROS                        PIC 9(3)V9(2).
           15 DCOMW84I-S-VTARIFA-CET                     PIC 9(15)V9(2).
           15 DCOMW84I-S-PTARIFA-CET                     PIC 9(3)V9(2).
           15 DCOMW84I-S-VPGTOSERV                       PIC 9(15)V9(2).
           15 DCOMW84I-S-PPGTOSERV                       PIC 9(3)V9(2).
           15 DCOMW84I-S-VREGISTROS                      PIC 9(15)V9(2).
           15 DCOMW84I-S-PREGISTROS                      PIC 9(3)V9(2).
           15 DCOMW84I-S-VCET-TOTAL                      PIC 9(15)V9(2).
           15 DCOMW84I-S-PCET-TOTAL                      PIC 9(3)V9(2).
           15 DCOMW84I-S-VCET-MES                        PIC 9(8)V9(7).
           15 DCOMW84I-S-VCET-ANO                        PIC 9(8)V9(7).
           15 DCOMW84I-S-VTOT-LIB                        PIC 9(15)V9(2).
           15 DCOMW84I-S-TOT-LIB                         PIC 9(3)V9(2).
           15 DCOMW84I-S-NOCOR                           PIC 9(2).
           15 DCOMW84I-S-AVAL-TESTEMUNHA OCCURS 04 TIMES.
              20 DCOMW84I-S-NOME-AVALISTA          PIC X(40).
              20 DCOMW84I-S-CNPJ-AVALISTA          PIC 9(09).
              20 DCOMW84I-S-FILIAL-AVALISTA        PIC 9(04).
              20 DCOMW84I-S-CTRL-AVALISTA          PIC 9(02).
              20 DCOMW84I-S-NOME-TESTEMUNHA        PIC X(40).
              20 DCOMW84I-S-CNPJ-TESTEMUNHA        PIC 9(09).
              20 DCOMW84I-S-FILIAL-TESTEMUNHA      PIC 9(04).
              20 DCOMW84I-S-CTRL-TESTEMUNHA        PIC 9(02).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
