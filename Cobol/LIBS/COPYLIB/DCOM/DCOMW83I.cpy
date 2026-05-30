      ******************************************************************
      * NOME BOOK : DCOMW83I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 31/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 1137 BYTES                                         *
      ******************************************************************
       05 DCOMW83I-HEADER.
         10 DCOMW83I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW83I'.
         10 DCOMW83I-TAM-LAYOUT        PIC 9(05)   VALUE 1137.
       05 DCOMW83I-REGISTRO.
         10 DCOMW83I-BLOCO-ENTRADA.
           15 DCOMW83I-E-NUM-OPER                       PIC 9(13).
         10 DCOMW83I-BLOCO-SAIDA.
           15 DCOMW83I-S-CAG-BCRIA                       PIC 9(5).
           15 DCOMW83I-S-DIGITO-AGENCIA                  PIC X(2).
           15 DCOMW83I-S-CCTA-BCRIA-CLI                  PIC 9(13).
           15 DCOMW83I-S-DIGITO-CONTA                    PIC X(2).
           15 DCOMW83I-S-CCNPJ-CPF                       PIC 9(9).
           15 DCOMW83I-S-CFLIAL-CNPJ                     PIC 9(4).
           15 DCOMW83I-S-CCTRL-CNPJ-CPF                  PIC 9(2).
           15 DCOMW83I-S-OPERACAO                        PIC 9(13).
           15 DCOMW83I-S-DINIC-OPER-DESC                 PIC X(10).
           15 DCOMW83I-S-VOPER-DESC-COML                 PIC 9(15)V9(2).
           15 DCOMW83I-S-NOME-AGEN                       PIC X(20).
           15 DCOMW83I-S-IPSSOA-DESC-COML                PIC X(40).
           15 DCOMW83I-S-VLIQ-OPER                       PIC 9(13)V9(2).
           15 DCOMW83I-S-PTX-JURO-MES                    PIC 9(07)V9(6).
           15 DCOMW83I-S-PTX-JURO-ANO                    PIC 9(07)V9(6).
           15 DCOMW83I-S-CCART-LIM                       PIC X(05).
           15 DCOMW83I-S-DATA-PRIM-VCTO                  PIC X(10).
           15 DCOMW83I-S-DVCTO-FNAL-OPER                 PIC X(10).
           15 DCOMW83I-S-VNOTA-PROMIS                    PIC 9(13)V9(2).
           15 DCOMW83I-S-VIOF-OPER                       PIC 9(13)V9(2).
           15 DCOMW83I-S-VTAC-OPER                       PIC 9(13)V9(2).
           15 DCOMW83I-S-VTARIF-REG                      PIC 9(15)V9(2).
           15 DCOMW83I-S-VTRIBUTOS                       PIC 9(15)V9(2).
           15 DCOMW83I-S-PTRIBUTOS                       PIC 9(3)V9(2).
           15 DCOMW83I-S-VSEGUROS                        PIC 9(15)V9(2).
           15 DCOMW83I-S-PSEGUROS                        PIC 9(3)V9(2).
           15 DCOMW83I-S-VTARIFA-CET                     PIC 9(15)V9(2).
           15 DCOMW83I-S-PTARIFA-CET                     PIC 9(3)V9(2).
           15 DCOMW83I-S-VPGTOSERV                       PIC 9(15)V9(2).
           15 DCOMW83I-S-PPGTOSERV                       PIC 9(3)V9(2).
           15 DCOMW83I-S-VREGISTROS                      PIC 9(15)V9(2).
           15 DCOMW83I-S-PREGISTROS                      PIC 9(3)V9(2).
           15 DCOMW83I-S-VCET-TOTAL                      PIC 9(15)V9(2).
           15 DCOMW83I-S-PCET-TOTAL                      PIC 9(3)V9(2).
           15 DCOMW83I-S-VCET-MES                        PIC 9(8)V9(7).
           15 DCOMW83I-S-VCET-ANO                        PIC 9(8)V9(7).
           15 DCOMW83I-S-VTOT-LIB                        PIC 9(15)V9(2).
           15 DCOMW83I-S-TOT-LIB                         PIC 9(3)V9(2).
           15 DCOMW83I-S-NOCOR                           PIC 9(2).
           15 DCOMW83I-S-AVAL-TESTEMUNHA OCCURS 04 TIMES.
              20 DCOMW83I-S-NOME-AVALISTA          PIC X(40).
              20 DCOMW83I-S-CNPJ-AVALISTA          PIC 9(09).
              20 DCOMW83I-S-FILIAL-AVALISTA        PIC 9(04).
              20 DCOMW83I-S-CTRL-AVALISTA          PIC 9(02).
              20 DCOMW83I-S-NOME-CONJUGE           PIC X(40).
              20 DCOMW83I-S-CNPJ-CNJG              PIC 9(09).
              20 DCOMW83I-S-FILIAL-CNJG            PIC 9(04).
              20 DCOMW83I-S-CTRL-CNJG              PIC 9(02).
              20 DCOMW83I-S-NOME-TESTEMUNHA        PIC X(40).
              20 DCOMW83I-S-CNPJ-TESTEMUNHA        PIC 9(09).
              20 DCOMW83I-S-FILIAL-TESTEMUNHA      PIC 9(04).
              20 DCOMW83I-S-CTRL-TESTEMUNHA        PIC 9(02).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
