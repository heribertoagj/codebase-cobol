      ***===========================================================***
      *** NOME INC                                     LENGTH=0180  ***
      *** I#SLIGIC                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - TCOCEN_EMITT_CHEQ
      ***===========================================================***
       05 SLIGIC-REGISTRO.
         10 SLIGIC-ATU-REG                        PIC X(01).
         10 SLIGIC-TIMESTAMP                      PIC X(26).
         10 SLIGIC-EMPRESA                        PIC 9(05).
         10 SLIGIC-CNPJCPF-CLI                    PIC X(09).
         10 SLIGIC-TPPES-CLI                      PIC X(01).
         10 SLIGIC-CNPJCPF-EMIT                   PIC X(09).
         10 SLIGIC-TPPES-EMIT                     PIC X(01).
         10 SLIGIC-DATAREF                        PIC X(10).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         10 SLIGIC-TAB-CARTEIRA OCCURS 3.
           15 SLIGIC-PCOCEN                       PIC 9(03)V99.
           15 SLIGIC-PCOCEN-SM                    PIC X(01).
           15 SLIGIC-VABERT                       PIC 9(13)V99.
           15 SLIGIC-VABERT-SM                    PIC X(01).
         10 FILLER                                PIC X(052).
