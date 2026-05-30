      ******************************************************************
      * SISTEMA : DCOM - I#DCOMP4                                      *
      * TAMANHO : 0079 BYTES                                           *
      * ARQUIVO : SIMULACAO DIGITACAO DESCONTO COMERCIAL               *
      * INCLUDE DA TABELA DCOMB0E2 - DB2PRD.TSMULA_DIGTC_DESC          *
      ******************************************************************
       01  P4-E2-REGISTRO.
01 05      05 P4-E2-NSMULA-OPER-DESC     PIC S9(9)V USAGE COMP-3.
06 03      05 P4-E2-NSMULA-DIGTC-DESC    PIC S9(5)V USAGE COMP-3.
09 10      05 P4-E2-DFLUXO-SMULA-DIGTC   PIC X(10).
19 09      05 P4-E2-VFLUXO-SMULA-DIGTC   PIC S9(15)V9(2) USAGE COMP-3.
28 03      05 P4-E2-TFLUXO-SMULA-DIGTC   PIC S9(5)V USAGE COMP-3.
31 26      05 P4-E2-HULT-ATULZ           PIC X(26).
57 05      05 P4-E2-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
62 08      05 P4-E2-CTERM                PIC X(8).
70 10      05 P4-E2-DINCL-REG            PIC X(10).
