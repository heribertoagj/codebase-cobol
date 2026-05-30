      ***===========================================================***
      *** NOME INC                                     LENGTH=0600  ***
      *** I#SLIGIS                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - SLIGB028-HIST_SACDO_TITLO
      **                           SLIGB032-SACDO_TITLO_CONSL
      **                                                             **
      ***===========================================================***
       05 SLIGIS-REGISTRO.
         10 SLIGIS-ATU-REG                        PIC X(001).
         10 SLIGIS-TIMESTAMP                      PIC X(026).
         10 SLIGIS-EMPRESA                        PIC 9(005).
         10 SLIGIS-CNPJCPF-SAC                    PIC X(009).
         10 SLIGIS-TPPES-SAC                      PIC X(001).
         10 SLIGIS-DATAI                          PIC X(010).
         10 SLIGIS-DATAF                          PIC X(010).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         10 SLIGIS-TAB-CARTEIRA OCCURS 3.
           20 SLIGIS-LIQ-FNAL                     PIC 9(03)V99.
           20 SLIGIS-LIQ-FNAL-SM                  PIC X(01).
           20 SLIGIS-LIQ-VENCTO-CART              PIC 9(03)V99.
           20 SLIGIS-LIQ-VENCTO-CART-SM           PIC X(01).
           20 SLIGIS-LIQ-VENCDOS-ANT              PIC 9(03)V99.
           20 SLIGIS-LIQ-VENCDOS-ANT-SM           PIC X(01).
           20 SLIGIS-LIQ-BXS-ANTPADAS             PIC 9(03)V99.
           20 SLIGIS-LIQ-BXS-ANTPADAS-SM          PIC X(01).
           20 SLIGIS-LIQ-C-BXS-ANTPADAS           PIC 9(03)V99.
           20 SLIGIS-LIQ-C-BXS-ANTPADAS-SM        PIC X(01).
           20 SLIGIS-LIQ-PRORROG                  PIC 9(03)V99.
           20 SLIGIS-LIQ-PRORROG-SM               PIC X(01).
           20 SLIGIS-LIQ-ABTMENTOS                PIC 9(03)V99.
           20 SLIGIS-LIQ-ABTMENTOS-SM             PIC X(01).
           20 SLIGIS-LIQ-PAGTO-ANTCP              PIC 9(03)V99.
           20 SLIGIS-LIQ-PAGTO-ANTCP-SM           PIC X(01).
           20 SLIGIS-VL-TOT-CART                  PIC 9(13)V99.
           20 SLIGIS-VL-TOT-VENCTO                PIC 9(13)V99.
           20 SLIGIS-VL-TOT-VENCDOS-ANT           PIC 9(13)V99.
           20 SLIGIS-VL-TOT-PAGTO-CART            PIC 9(13)V99.
           20 SLIGIS-VL-TOT-BXS-ANTCIPDAS         PIC 9(13)V99.
           20 SLIGIS-VL-TOT-PRORROG               PIC 9(13)V99.
           20 SLIGIS-VL-TOT-ABTMENTOS             PIC 9(13)V99.
           20 SLIGIS-VL-TOT-PAGTO-ANTCP           PIC 9(13)V99.
         10 FILLER                                PIC X(034).
