      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQ3                                      *
      * TAMANHO : 0075 BYTES                                           *
      * ARQUIVO : INSTRUCAO DESCONTO COMERCIAL                         *
      * INCLUDE DA TABELA DCOMB0C2 - DB2PRD.TINSTR_DESC_PCELA          *
      ******************************************************************
       01  Q3-C2-REGISTRO.
01 03      05 Q3-C2-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 Q3-C2-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 03      05 Q3-C2-NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
12 02      05 Q3-C2-NSEQ-INSTR-PCELA     PIC S9(2)V USAGE COMP-3.
14 10      05 Q3-C2-DINSTR-DESC-PCELA    PIC X(10).
24 09      05 Q3-C2-VINSTR-DESC-PCELA    PIC S9(15)V9(2) USAGE COMP-3.
33 04      05 Q3-C2-PINSTR-DESC-PCELA    PIC S9(3)V9(3) USAGE COMP-3.
37 26      05 Q3-C2-HULT-ATULZ           PIC X(26).
63 05      05 Q3-C2-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
68 08      05 Q3-C2-CTERM                PIC X(8).
