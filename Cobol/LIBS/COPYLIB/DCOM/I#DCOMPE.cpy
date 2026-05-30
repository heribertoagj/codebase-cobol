      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPE                                      *
      * TAMANHO : 0072 BYTES                                           *
      * ARQUIVO : TRANSITORIA CARTEIRA COBRANCA                        *
      * INCLUDE DA TABELA DCOMB0F4 - DB2PRD.TTRASI_CART_COBR           *
      ******************************************************************
       01  PE-F4-REGISTRO.
01 03      05 PE-F4-DANO-OPER-DESC       PIC S9(4)V  USAGE COMP-3.
04 05      05 PE-F4-NSEQ-OPER-DESC       PIC S9(9)V  USAGE COMP-3.
09 02      05 PE-F4-CBCO-PRODT-COBR      PIC S9(3)V  USAGE COMP-3.
11 02      05 PE-F4-CIDTFD-PRODT-COBR    PIC S9(2)V  USAGE COMP-3.
13 10      05 PE-F4-CNEGOC-COBR          PIC S9(18)V USAGE COMP-3.
23 01      05 PE-F4-CINDCD-SELEC-DESC    PIC  X(1).
24 26      05 PE-F4-HULT-ATULZ           PIC  X(26).
50 05      05 PE-F4-CFUNC-BDSCO          PIC S9(9)V  USAGE COMP-3.
55 08      05 PE-F4-CTERM                PIC  X(08).
63 10      05 PE-F4-DINCL-REG            PIC  X(10).
