      ***===========================================================***
      *** NOME INC                                     LENGTH=0580  ***
      *** I#SLIGIK                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - SLIGB005-CLI_TITLO_CONSL
      **                           SLIGB018-HIST_CLI_TITLO
      **                                                             **
      ***===========================================================***
       05 SLIGIK-REGISTRO.
         10 SLIGIK-ATU-REG                        PIC X(001).
         10 SLIGIK-TIMESTAMP                      PIC X(026).
         10 SLIGIK-EMPRESA                        PIC 9(005).
         10 SLIGIK-CNPJCPF-CLI                    PIC X(009).
         10 SLIGIK-TPPES-CLI                      PIC X(001).
         10 SLIGIK-DATAI                          PIC X(010).
         10 SLIGIK-DATAF                          PIC X(010).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         10 SLIGIK-TAB-CARTEIRA OCCURS 3.
           15 SLIGIK-LIQ-FNAL                     PIC 9(03)V99.
           15 SLIGIK-LIQ-FNAL-SM                  PIC X(01).
           15 SLIGIK-LIQ-VENCTO-CART              PIC 9(03)V99.
           15 SLIGIK-LIQ-VENCTO-CART-SM           PIC X(01).
           15 SLIGIK-LIQ-VENCDOS-ANT              PIC 9(03)V99.
           15 SLIGIK-LIQ-VENCDOS-ANT-SM           PIC X(01).
           15 SLIGIK-LIQ-BXS-ANTPADAS             PIC 9(03)V99.
           15 SLIGIK-LIQ-BXS-ANTPADAS-SM          PIC X(01).
           15 SLIGIK-LIQ-C-BXS-ANTPADAS           PIC 9(03)V99.
           15 SLIGIK-LIQ-C-BXS-ANTPADAS-SM        PIC X(01).
           15 SLIGIK-LIQ-PRORROG                  PIC 9(03)V99.
           15 SLIGIK-LIQ-PRORROG-SM               PIC X(01).
           15 SLIGIK-LIQ-ABTMENTOS                PIC 9(03)V99.
           15 SLIGIK-LIQ-ABTMENTOS-SM             PIC X(01).
           15 SLIGIK-VL-TOT-CART                  PIC 9(13)V99.
           15 SLIGIK-VL-TOT-VENCTO                PIC 9(13)V99.
           15 SLIGIK-VL-TOT-VENCDOS-ANT           PIC 9(13)V99.
           15 SLIGIK-VL-TOT-PAGTO-CART            PIC 9(13)V99.
           15 SLIGIK-VL-TOT-BXS-ANTCIPDAS         PIC 9(13)V99.
           15 SLIGIK-VL-TOT-PRORROG               PIC 9(13)V99.
           15 SLIGIK-VL-TOT-ABTMENTOS             PIC 9(13)V99.
           15 SLIGIK-VL-TOT-PGT-ANTCIPDOS         PIC 9(13)V99.
         10 FILLER                                PIC X(032).
