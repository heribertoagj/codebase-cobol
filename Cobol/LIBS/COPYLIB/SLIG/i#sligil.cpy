      ***===========================================================***
      *** NOME INC                                     LENGTH=0180  ***
      *** I#SLIGIL                                                  ***
      ***-----------------------------------------------------------***
      **  DESCRICAO: SISTEMA DE INDICES DE LIQUIDEZ - SLIG           **
      **             ARQUIVOS BI - SLIGB077-TCOCEN_TITLO_VENCD
      **                           SLIGB078-TCOCEN_TITLO_VNCER
      **                           SLIGB040-TCOCEN_SACDO_TITLO
      **                                                             **
      ***===========================================================***
       05 SLIGIL-REGISTRO.
         15 SLIGIL-ATU-REG                        PIC X(001).
         15 SLIGIL-TIMESTAMP                      PIC X(026).
         15 SLIGIL-EMPRESA                        PIC 9(005).
         15 SLIGIL-CNPJCPF-CLI                    PIC X(009).
         15 SLIGIL-TPPES-CLI                      PIC X(001).
         15 SLIGIL-CNPJCPF-SAC                    PIC X(009).
         15 SLIGIL-TPPES-SAC                      PIC X(001).
         15 SLIGIL-DATAREF                        PIC X(010).
      *---- (1)SIMPLES - (2)CAUCIONADA - (3)DESCONTO
         15 SLIGIL-TAB-CARTEIRA OCCURS 3.
           25 SLIGIL-LIQ-FNAL                     PIC 9(03)V99.
           25 SLIGIL-LIQ-FNAL-SM                  PIC X(01).
         15 FILLER                                PIC X(080).
