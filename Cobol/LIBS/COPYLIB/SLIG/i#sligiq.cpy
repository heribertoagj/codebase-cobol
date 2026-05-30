      ***===========================================================***
      *** NOME INC                                     LENGTH=0180  ***
      *** I#SLIGIQ                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - TGRP_SACDO_TITLO
      **                                                             **
      ***===========================================================***
       05 SLIGIQ-REGISTRO.
         10 SLIGIQ-ATU-REG                        PIC X(01).
         10 SLIGIQ-TIMESTAMP                      PIC X(26).
         10 SLIGIQ-EMPRESA                        PIC 9(05).
         10 SLIGIQ-CGRP-ECONM                     PIC 9(09).
         10 SLIGIQ-CNPJCPF-SAC                    PIC X(09).
         10 SLIGIQ-TPPES-SAC                      PIC X(01).
         10 SLIGIQ-DATAI                          PIC X(10).
         10 SLIGIQ-DATAF                          PIC X(10).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         10 SLIGIQ-TAB-CARTEIRA OCCURS 3.
           15 SLIGIQ-LIQ-FNAL                     PIC 9(03)V99.
           15 SLIGIQ-LIQ-FNAL-SM                  PIC X(01).
         10 FILLER                                PIC X(091).
