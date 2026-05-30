      ***===========================================================***
      *** NOME INC                                     LENGTH=0630  ***
      *** I#SLIGIA                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - SLIGB017-HIST_CLI_CHEQ
      **                         - SLIGB001-CLI_CHEQ_CONSL
      ***===========================================================***
       05 SLIGIA-REGISTRO.
         10 SLIGIA-ATU-REG                        PIC X(01).
         10 SLIGIA-TIMESTAMP                      PIC X(26).
         10 SLIGIA-EMPRESA                        PIC 9(05).
         10 SLIGIA-CNPJCPF-CLI                    PIC X(09).
         10 SLIGIA-TPPES-CLI                      PIC X(01).
         10 SLIGIA-DATAI                          PIC X(10).
         10 SLIGIA-DATAF                          PIC X(10).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         10 SLIGIA-TAB-CARTEIRA OCCURS 3.
           20 SLIGIA-LIQ-FNAL                     PIC 9(03)V99.
           20 SLIGIA-LIQ-FNAL-SM                  PIC X(01).
           20 SLIGIA-LIQ-PRIM-DEP                 PIC 9(03)V99.
           20 SLIGIA-LIQ-PRIM-DEP-SM              PIC X(01).
           20 SLIGIA-LIQ-REAPTACAO                PIC 9(03)V99.
           20 SLIGIA-LIQ-REAPTACAO-SM             PIC X(01).
           20 SLIGIA-LIQ-VENCTO-PER               PIC 9(03)V99.
           20 SLIGIA-LIQ-VENCTO-PER-SM            PIC X(01).
           20 SLIGIA-LIQ-CHQ-DEPOSITADOS          PIC 9(03)V99.
           20 SLIGIA-LIQ-CHQ-DEPOSITADOS-SM       PIC X(01).
           20 SLIGIA-LIQ-BXS-ANTCIPDAS            PIC 9(03)V99.
           20 SLIGIA-LIQ-BXS-ANTCIPDAS-SM         PIC X(01).
           20 SLIGIA-LIQ-CHQ-REAPDOS              PIC 9(03)V99.
           20 SLIGIA-LIQ-CHQ-REAPDOS-SM           PIC X(01).
           20 SLIGIA-VL-TOT-CART-CHQ              PIC 9(13)V99.
           20 SLIGIA-VL-TOT-CHQ-VENC-PER          PIC 9(13)V99.
           20 SLIGIA-VL-TOT-DEP-PER               PIC 9(13)V99.
           20 SLIGIA-VL-TOT-CHQ-PAGOS             PIC 9(13)V99.
           20 SLIGIA-VL-TOT-CHQ-PRIM-DEV          PIC 9(13)V99.
           20 SLIGIA-VL-TOT-CHQ-REAPDOS           PIC 9(13)V99.
           20 SLIGIA-VL-TOT-CHQ-REAPDOS-PG        PIC 9(13)V99.
           20 SLIGIA-VL-TOT-CHQ-REAPDOS-DV        PIC 9(13)V99.
           20 SLIGIA-VL-TOT-BXS-ANTCIPDAS         PIC 9(13)V99.
         10 FILLER                                PIC X(037).

