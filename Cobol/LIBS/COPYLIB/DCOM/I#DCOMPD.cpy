      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPD                                      *
      * TAMANHO : 0102 BYTES                                           *
      * ARQUIVO : TRANSITORIA BORDERO CUSTODIA DESCONTO                *
      * INCLUDE DA TABELA DCOMB0F3 - DB2PRD.TTRASI_BDERO_CTDIA         *
      ******************************************************************
       01  PD-F3-REGISTRO.
01 03      05 PD-F3-DANO-OPER-DESC       PIC S9(4)V  USAGE COMP-3.
04 05      05 PD-F3-NSEQ-OPER-DESC       PIC S9(9)V  USAGE COMP-3.
09 02      05 PD-F3-CBCO-CLI-CHEQC       PIC S9(3)V  USAGE COMP-3.
11 03      05 PD-F3-CDEPDC               PIC S9(5)V  USAGE COMP-3.
14 07      05 PD-F3-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
21 03      05 PD-F3-CFLIAL-EMPR-CHEQC    PIC S9(5)V  USAGE COMP-3.
24 04      05 PD-F3-CPROT-LOTE-CHEQC     PIC S9(7)V  USAGE COMP-3.
28 10      05 PD-F3-DMOVTO               PIC X(10).
38 05      05 PD-F3-QCHEQ-BDERO          PIC S9(9)V  USAGE COMP-3.
43 08      05 PD-F3-VTOT-CHEQC-BDERO     PIC S9(13)V9(2) USAGE COMP-3.
51 01      05 PD-F3-CSIT-BDERO           PIC S9(1)V USAGE COMP-3.
52 01      05 PD-F3-CINDCD-SELEC-DESC    PIC X(1).
53 26      05 PD-F3-HULT-ATULZ           PIC X(26).
79 05      05 PD-F3-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
84 08      05 PD-F3-CTERM                PIC X(8).
92 10      05 PD-F3-DINCL-REG            PIC X(10).
10201      05 PD-F3-CSIT-PIOR-CHEQ       PIC S9(01)V USAGE COMP-3.
