      ***===========================================================***
      *** NOME INC                                     LENGTH=0260  ***
      *** I#SLIGIR                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - SACDO_TITLO_CONSL                 **
      **                                                             **
      ***===========================================================***
       05 SLIGIR-REGISTRO.
         10 SLIGIR-ATU-REG                       PIC X(01).
         10 SLIGIR-TIMESTAMP                     PIC X(26).
         10 SLIGIR-EMPRESA                       PIC 9(05).
         10 SLIGIR-CNPJCPF-SAC                   PIC X(09).
         10 SLIGIR-TPPES-SAC                     PIC X(01).
         10 SLIGIR-DATAI                         PIC X(10).
         10 SLIGIR-DATAF                         PIC X(10).
         10 SLIGIR-LIQ-FNAL                      PIC 9(03)V99.
         10 SLIGIR-LIQ-FNAL-SM                   PIC X(01).
         10 SLIGIR-LIQ-VENCTOS-CART              PIC 9(03)V99.
         10 SLIGIR-LIQ-VENCTOS-CART-SM           PIC X(01).
         10 SLIGIR-LIQ-VENCTOS-ANT               PIC 9(03)V99.
         10 SLIGIR-LIQ-VENCTOS-ANT-SM            PIC X(01).
         10 SLIGIR-LIQ-BXS-ANTPADAS              PIC 9(03)V99.
         10 SLIGIR-LIQ-BXS-ANTPADAS-SM           PIC X(01).
         10 SLIGIR-LIQ-C-BXS-ANTPADAS            PIC 9(03)V99.
         10 SLIGIR-LIQ-C-BXS-ANTPADAS-SM         PIC X(01).
         10 SLIGIR-LIQ-PRORROG                   PIC 9(03)V99.
         10 SLIGIR-LIQ-PRORROG-SM                PIC X(01).
         10 SLIGIR-LIQ-ABTMENTOS                 PIC 9(03)V99.
         10 SLIGIR-LIQ-ABTMENTOS-SM              PIC X(01).
         10 SLIGIR-LIQ-PG-ANTCPADOS              PIC 9(03)V99.
         10 SLIGIR-LIQ-PG-ANTCPADOS-SM           PIC X(01).
         10 SLIGIR-VL-TOT-CART                   PIC 9(13)V99.
         10 SLIGIR-VL-TOT-VENCTOS-CART           PIC 9(13)V99.
         10 SLIGIR-VL-TOT-VENCDOS-ANT            PIC 9(13)V99.
         10 SLIGIR-VL-TOT-PAGTOS-CART            PIC 9(13)V99.
         10 SLIGIR-VL-TOT-BXS-ANTPDAS            PIC 9(13)V99.
         10 SLIGIR-VL-TOT-PRORROG                PIC 9(13)V99.
         10 SLIGIR-VL-TOT-ABTMENTOS              PIC 9(13)V99.
         10 SLIGIR-VL-TOT-PG-ANTCPADOS           PIC 9(13)V99.
         10 FILLER                               PIC X(030).
