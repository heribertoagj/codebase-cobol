      ***===========================================================***
      *** NOME INC                                     LENGTH=650   ***
      *** I#SLIGIE                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - HIST_SACDO_CHEQ                   **
      ***===========================================================***
       05 SLIGIE-REGISTRO.
         10 SLIGIE-ATU-REG                        PIC X(01).
         10 SLIGIE-TIMESTAMP                      PIC X(26).
         10 SLIGIE-EMPRESA                        PIC 9(05).
         10 SLIGIE-CNPJCPF-EMIT                   PIC X(09).
         10 SLIGIE-TPPES-EMIT                     PIC X(01).
         10 SLIGIE-DATAI                          PIC X(10).
         10 SLIGIE-DATAF                          PIC X(10).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         10 SLIGIE-TAB-CARTEIRA OCCURS 3.
           20 SLIGIE-LIQ-FNAL                     PIC 9(03)V99.
           20 SLIGIE-LIQ-FNAL-SM                  PIC X(01).
           20 SLIGIE-LIQ-PRIM-DEP                 PIC 9(03)V99.
           20 SLIGIE-LIQ-PRIM-DEP-SM              PIC X(01).
           20 SLIGIE-LIQ-REAPTACAO                PIC 9(03)V99.
           20 SLIGIE-LIQ-REAPTACAO-SM             PIC X(01).
           20 SLIGIE-LIQ-VENCTO-PER               PIC 9(03)V99.
           20 SLIGIE-LIQ-VENCTO-PER-SM            PIC X(01).
           20 SLIGIE-LIQ-CHQ-DEPOSITADOS          PIC 9(03)V99.
           20 SLIGIE-LIQ-CHQ-DEPOSITADOS-SM       PIC X(01).
           20 SLIGIE-LIQ-BXS-ANTCIPDAS            PIC 9(03)V99.
           20 SLIGIE-LIQ-BXS-ANTCIPDAS-SM         PIC X(01).
           20 SLIGIE-LIQ-CHQ-REAPDOS              PIC 9(03)V99.
           20 SLIGIE-LIQ-CHQ-REAPDOS-SM           PIC X(01).
           20 SLIGIE-VL-TOT-CART-CHQ              PIC 9(13)V99.
           20 SLIGIE-VL-TOT-CHQ-VENC-PER          PIC 9(13)V99.
           20 SLIGIE-VL-TOT-DEP-PER               PIC 9(13)V99.
           20 SLIGIE-VL-TOT-CHQ-PAGOS             PIC 9(13)V99.
           20 SLIGIE-VL-TOT-CHQ-PRIM-DEV          PIC 9(13)V99.
           20 SLIGIE-VL-TOT-CHQ-REAPDOS           PIC 9(13)V99.
           20 SLIGIE-VL-TOT-CHQ-REAPDOS-PG        PIC 9(13)V99.
           20 SLIGIE-VL-TOT-CHQ-REAPDOS-DV        PIC 9(13)V99.
           20 SLIGIE-VL-TOT-BXS-ANTCIPDAS         PIC 9(13)V99.
         10 FILLER                                PIC X(057).
