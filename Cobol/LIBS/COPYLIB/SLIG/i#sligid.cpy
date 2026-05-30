      ***===========================================================***
      *** NOME INC                                     LENGTH=0250  ***
      *** I#SLIGID                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - SACDO_CHEQ_CONSL
      **                                                             **
      ***===========================================================***
       05 SLIGID-REGISTRO.
         10 SLIGID-ATU-REG                        PIC X(01).
         10 SLIGID-TIMESTAMP                      PIC X(26).
         10 SLIGID-EMPRESA                        PIC 9(05).
         10 SLIGID-CNPJCPF-EMIT                   PIC X(09).
         10 SLIGID-TPPES-EMIT                     PIC X(01).
         10 SLIGID-DATAI                          PIC X(10).
         10 SLIGID-DATAF                          PIC X(10).
         10 SLIGID-LIQ-FNAL                       PIC 9(03)V99.
         10 SLIGID-LIQ-FNAL-SM                    PIC X(01).
         10 SLIGID-LIQ-PRIM-DEP                   PIC 9(03)V99.
         10 SLIGID-LIQ-PRIM-DEP-SM                PIC X(01).
         10 SLIGID-LIQ-REAPTACAO                  PIC 9(03)V99.
         10 SLIGID-LIQ-REAPTACAO-SM               PIC X(01).
         10 SLIGID-LIQ-VENCTO-PER                 PIC 9(03)V99.
         10 SLIGID-LIQ-VENCTO-PER-SM              PIC X(01).
         10 SLIGID-LIQ-CHQ-DEPOSITADOS            PIC 9(03)V99.
         10 SLIGID-LIQ-CHQ-DEPOSITADOS-SM         PIC X(01).
         10 SLIGID-LIQ-BXS-ANTCIPDAS              PIC 9(03)V99.
         10 SLIGID-LIQ-BXS-ANTCIPDAS-SM           PIC X(01).
         10 SLIGID-LIQ-CHQ-REAPDOS                PIC 9(03)V99.
         10 SLIGID-LIQ-CHQ-REAPDOS-SM             PIC X(01).
         10 SLIGID-VL-TOT-CART-CHQ                PIC 9(13)V99.
         10 SLIGID-VL-TOT-CHQ-VENC-PER            PIC 9(13)V99.
         10 SLIGID-VL-TOT-DEP-PER                 PIC 9(13)V99.
         10 SLIGID-VL-TOT-CHQ-PAGOS               PIC 9(13)V99.
         10 SLIGID-VL-TOT-CHQ-PRIM-DEV            PIC 9(13)V99.
         10 SLIGID-VL-TOT-CHQ-REAPDOS             PIC 9(13)V99.
         10 SLIGID-VL-TOT-CHQ-REAPDOS-PG          PIC 9(13)V99.
         10 SLIGID-VL-TOT-CHQ-REAPDOS-DV          PIC 9(13)V99.
         10 SLIGID-VL-TOT-BXS-ANTCIPDAS           PIC 9(13)V99.
         10 FILLER                                PIC X(041).
