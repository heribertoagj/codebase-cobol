      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPM                                      *
      * TAMANHO : 0106 BYTES                                           *
      * ARQUIVO : TRANSITORIA LOTE CUSTODIA DESCONTO                   *
      * INCLUDE DA TABELA DCOMB0G3 - DB2PRD.TTRASI_LOTE_CTDIA          *
      ******************************************************************
       01 PM-G3-REGISTRO.
01 03     05 PM-G3-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05     05 PM-G3-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02     05 PM-G3-CBCO-CLI-CHEQC       PIC S9(3)V USAGE COMP-3.
11 03     05 PM-G3-CDEPDC               PIC S9(5)V USAGE COMP-3.
14 07     05 PM-G3-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
21 03     05 PM-G3-CFLIAL-EMPR-CHEQC    PIC S9(5)V USAGE COMP-3.
24 04     05 PM-G3-CPROT-LOTE-CHEQC     PIC S9(7)V USAGE COMP-3.
28 10     05 PM-G3-DMOVTO               PIC X(10).
38 04     05 PM-G3-CLOTE-CHEQC          PIC S9(7)V USAGE COMP-3.
42 05     05 PM-G3-QCHEQ-LOTE           PIC S9(9)V USAGE COMP-3.
47 09     05 PM-G3-VTOT-CHEQC-LOTE      PIC S9(15)V9(2) USAGE COMP-3.
56 01     05 PM-G3-CINDCD-SELEC-DESC    PIC X(1).
57 26     05 PM-G3-HULT-ATULZ           PIC X(26).
83 05     05 PM-G3-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
88 08     05 PM-G3-CTERM                PIC X(8).
96 10     05 PM-G3-DINCL-REG            PIC X(10).
10601     05 PM-G3-CSIT-PIOR-CHEQ       PIC S9(01)V USAGE COMP-3.
