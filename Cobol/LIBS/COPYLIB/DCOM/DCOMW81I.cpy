      ******************************************************************
      * NOME BOOK : DCOMW81I - BOOK DE INTERFACE COM SERVICO FUNCIONAL *
      * DATA      : 20/10/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 397 BYTES                                          *
      ******************************************************************
       05 DCOMW81I-HEADER.
         10 DCOMW81I-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW81I'.
         10 DCOMW81I-TAM-LAYOUT        PIC 9(05)   VALUE 397.
       05 DCOMW81I-REGISTRO.
         10 DCOMW81I-BLOCO-ENTRADA.
           15 DCOMW81I-E-NUM-OPER                       PIC 9(13).
         10 DCOMW81I-BLOCO-SAIDA.
           15 DCOMW81I-S-CAG-BCRIA                       PIC 9(5).
           15 DCOMW81I-S-DIGITO-AGENCIA                  PIC X(2).
           15 DCOMW81I-S-CCTA-BCRIA-CLI                  PIC 9(13).
           15 DCOMW81I-S-DIGITO-CONTA                    PIC X(2).
           15 DCOMW81I-S-CCNPJ-CPF                       PIC 9(9).
           15 DCOMW81I-S-CFLIAL-CNPJ                     PIC 9(4).
           15 DCOMW81I-S-CCTRL-CNPJ-CPF                  PIC 9(2).
           15 DCOMW81I-S-OPERACAO                        PIC 9(13).
           15 DCOMW81I-S-DINIC-OPER-DESC                 PIC X(10).
           15 DCOMW81I-S-VOPER-DESC-COML                 PIC 9(15)V9(2).
           15 DCOMW81I-S-IPSSOA-DESC-COML                PIC X(40).
           15 DCOMW81I-S-DATA-PRIM-VCTO                  PIC X(10).
           15 DCOMW81I-S-DVCTO-FNAL-OPER                 PIC X(10).
           15 DCOMW81I-S-PTX-JURO-MES                    PIC 9(07)V9(6).
           15 DCOMW81I-S-PTX-JURO-ANO                    PIC 9(07)V9(6).
           15 DCOMW81I-S-QTITLO-CHEQ-PCELA               PIC 9(9).
           15 DCOMW81I-S-VTRIBUTOS                       PIC 9(15)V9(2).
           15 DCOMW81I-S-PTRIBUTOS                       PIC 9(3)V9(2).
           15 DCOMW81I-S-VSEGUROS                        PIC 9(15)V9(2).
           15 DCOMW81I-S-PSEGUROS                        PIC 9(3)V9(2).
           15 DCOMW81I-S-VTARIFA-CET                     PIC 9(15)V9(2).
           15 DCOMW81I-S-PTARIFA-CET                     PIC 9(3)V9(2).
           15 DCOMW81I-S-VPGTOSERV                       PIC 9(15)V9(2).
           15 DCOMW81I-S-PPGTOSERV                       PIC 9(3)V9(2).
           15 DCOMW81I-S-VREGISTROS                      PIC 9(15)V9(2).
           15 DCOMW81I-S-PREGISTROS                      PIC 9(3)V9(2).
           15 DCOMW81I-S-VCET-TOTAL                      PIC 9(15)V9(2).
           15 DCOMW81I-S-PCET-TOTAL                      PIC 9(3)V9(2).
           15 DCOMW81I-S-VCET-MES                        PIC 9(8)V9(7).
           15 DCOMW81I-S-VCET-ANO                        PIC 9(8)V9(7).
           15 DCOMW81I-S-VTOT-LIQ                        PIC 9(13)V9(2).
           15 DCOMW81I-S-VTOT-LIB                        PIC 9(15)V9(2).
           15 DCOMW81I-S-TOT-LIB                         PIC 9(3)V9(2).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
