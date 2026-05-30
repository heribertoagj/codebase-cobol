      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPT                                      *
      * TAMANHO : 0079 BYTES                                           *
      * ARQUIVO : TRANSITORIA REUTILIZACAO INSTRUCAO DESCONTO          *
      * INCLUDE DA TABELA DCOMB0I1 - DB2PRD.TTRASI_RUTIL_INSTR         *
      ******************************************************************
       01  PT-I1-REGISTRO.
01 02      05  PT-I1-CBCO                 PIC S9(3)V      USAGE COMP-3.
03 03      05  PT-I1-CAG-BCRIA            PIC S9(5)V      USAGE COMP-3.
06 07      05  PT-I1-CCTA-BCRIA-CLI       PIC S9(13)V     USAGE COMP-3.
13 03      05  PT-I1-NTITLO-DESC-COML     PIC S9(5)V      USAGE COMP-3.
16 02      05  PT-I1-NSEQ-INSTR-PCELA     PIC S9(2)V      USAGE COMP-3.
18 10      05  PT-I1-DINSTR-DESC-PCELA    PIC X(10).
28 09      05  PT-I1-VINSTR-DESC-PCELA    PIC S9(15)V9(2) USAGE COMP-3.
37 04      05  PT-I1-PINSTR-DESC-PCELA    PIC S9(3)V9(3)  USAGE COMP-3.
41 26      05  PT-I1-HULT-ATULZ           PIC X(26).
67 05      05  PT-I1-CFUNC-BDSCO          PIC S9(9)V      USAGE COMP-3.
72 08      05  PT-I1 CTERM                PIC X(8).
