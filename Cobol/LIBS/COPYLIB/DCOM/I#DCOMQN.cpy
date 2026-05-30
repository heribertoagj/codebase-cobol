      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQN                                      *
      * TAMANHO : 0075 BYTES                                           *
      * ARQUIVO : HISTORICO INSTRUCAO DE DESCONTO PARCELA              *
      * INCLUDE DA TABELA DCOMB0B3 - DB2PRD.THIST_INSTR_DESC           *
      ******************************************************************
       01  QN-B3-REGISTRO.
01 03      05 QN-B3-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 QN-B3-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02      05 QN-B3-NSEQ-INSTR-PCELA     PIC S9(2)V USAGE COMP-3.
11 03      05 QN-B3-NTITLO-DESC-COML     PIC S9(5)V USAGE COMP-3.
14 26      05 QN-B3-HULT-ATULZ           PIC X(26).
40 10      05 QN-B3-DINSTR-DESC-PCELA    PIC X(10).
50 09      05 QN-B3-VINSTR-DESC-PCELA    PIC S9(15)V9(2) USAGE COMP-3.
59 04      05 QN-B3-PINSTR-DESC-PCELA    PIC S9(3)V9(3) USAGE COMP-3.
63 05      05 QN-B3-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
68 08      05 QN-B3-CTERM                PIC X(8).
