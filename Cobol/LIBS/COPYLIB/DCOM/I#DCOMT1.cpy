      ******************************************************************
      * SISTEMA : DCOM - I#DCOMT1                                      *
      * TAMANHO : 0070 BYTES                                           *
      * ARQUIVO : TESTEMUNHA AGENCIA                                   *
      * INCLUDE DA TABELA DCOMB0K9 - DB2PRD.TTESTM_AG_OPERL            *
      ******************************************************************
       01  T1-K9-REGISTRO.
01 02      05  T1-K9-CBCO                 PIC S9(3)V USAGE COMP-3.
03 03      05  T1-K9-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
06 05      05  T1-K9-NPSSOA-DESC-COML     PIC S9(9)V USAGE COMP-3.
11 10      05  T1-K9-DULT-UTILZ           PIC X(10).
21 01      05  T1-K9-DULT-UTILZ-NULL      PIC X(01).
22 26      05  T1-K9-HULT-ATULZ           PIC X(26).
48 10      05  T1-K9-DINCL-REG            PIC X(10).
58 05      05  T1-K9-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
63 08      05  T1-K9-CTERM                PIC X(8).

