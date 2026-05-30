      ***===========================================================***
      *** NOME INC                                     LENGTH=0120  ***
      *** I#SLIGII                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - TGRP_EMITT_CHEQ                   **
      **                                                             **
      ***===========================================================***
       05 SLIGII-REGISTRO.
         10 SLIGII-ATU-REG                        PIC X(01).
         10 SLIGII-TIMESTAMP                      PIC X(26).
         10 SLIGII-EMPRESA                        PIC 9(05).
         10 SLIGII-CGRP-ECONM                     PIC 9(09).
         10 SLIGII-CNPJCPF-SAC                    PIC X(09).
         10 SLIGII-TPPES-SAC                      PIC X(01).
         10 SLIGII-DATAI                          PIC X(10).
         10 SLIGII-DATAF                          PIC X(10).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         10 SLIGII-TAB-CARTEIRA OCCURS 3.
           15 SLIGII-LIQ-FNAL                     PIC 9(03)V99.
           15 SLIGII-LIQ-FNAL-SM                  PIC X(01).
         10 FILLER                                PIC X(031).
