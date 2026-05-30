      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPL                                      *
      * TAMANHO : 0075 BYTES                                           *
      * ARQUIVO : TRANSITORIA INSTRUCAO DESCONTO PARCELA               *
      * INCLUDE DA TABELA DCOMB0G2 - DB2PRD.TTRASI_INSTR_DESC          *
      ******************************************************************
       01  PL-G2-REGISTRO.
01 03      05 PL-G2-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 PL-G2-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 03      05 PL-G2-NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
12 02      05 PL-G2-NSEQ-INSTR-PCELA     PIC S9(2)V USAGE COMP-3.
14 10      05 PL-G2-DINSTR-DESC-PCELA    PIC X(10).
24 09      05 PL-G2-VINSTR-DESC-PCELA    PIC S9(15)V9(2) USAGE COMP-3.
33 04      05 PL-G2-PINSTR-DESC-PCELA    PIC S9(3)V9(3) USAGE COMP-3.
37 26      05 PL-G2-HULT-ATULZ           PIC X(26).
63 05      05 PL-G2-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
68 08      05 PL-G2-CTERM                PIC X(8).
