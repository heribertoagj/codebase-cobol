      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPO                                      *
      * TAMANHO : 0073 BYTES                                           *
      * ARQUIVO : TRANSITORIA OPERACAO CONTA CHEQUE                    *
      * INCLUDE DA TABELA DCOMB0H7 - DB2PRD.TTRASI_CTA_CHEQC           *
      ******************************************************************
       01  PO-H7-REGISTRO.
01 03      05 PO-H7-DANO-OPER-DESC       PIC S9(4)V  USAGE COMP-3.
04 05      05 PO-H7-NSEQ-OPER-DESC       PIC S9(9)V  USAGE COMP-3.
09 02      05 PO-H7-CBCO-CLI-CHEQC       PIC S9(3)V  USAGE COMP-3.
11 03      05 PO-H7-CDEPDC               PIC S9(5)V  USAGE COMP-3.
14 07      05 PO-H7-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
21 03      05 PO-H7-CFLIAL-EMPR-CHEQC    PIC S9(5)V  USAGE COMP-3.
24 01      05 PO-H7-CINDCD-SELEC-DESC    PIC X(1).
25 26      05 PO-H7-HULT-ATULZ           PIC X(26).
51 05      05 PO-H7-CFUNC-BDSCO          PIC S9(9)V  USAGE COMP-3.
56 08      05 PO-H7-CTERM                PIC X(8).
64 10      05 PO-H7-DINCL-REG            PIC X(10).
