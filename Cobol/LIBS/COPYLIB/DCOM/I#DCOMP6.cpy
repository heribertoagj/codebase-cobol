      ******************************************************************
      * SISTEMA : DCOM - I#DCOMP6                                      *
      * TAMANHO : 0102 BYTES                                           *
      * ARQUIVO : SIMULACAO CUSTODIA DESCONTO                          *
      * INCLUDE DA TABELA DCOMB0E4 - DB2PRD.TSMULA_LOTE_CTDIA          *
      ******************************************************************
       01  P6-E4-REGISTRO.
 01 05     05 P6-E4-NSMULA-OPER-DESC     PIC S9(9)V USAGE COMP-3.
 06 02     05 P6-E4-CBCO-CLI-CHEQC       PIC S9(3)V USAGE COMP-3.
 08 03     05 P6-E4-CDEPDC               PIC S9(5)V USAGE COMP-3.
 11 07     05 P6-E4-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
 18 03     05 P6-E4-CFLIAL-EMPR-CHEQC    PIC S9(5)V USAGE COMP-3.
 21 04     05 P6-E4-CPROT-LOTE-CHEQC     PIC S9(7)V USAGE COMP-3.
 25 10     05 P6-E4-DMOVTO               PIC X(10).
 35 04     05 P6-E4-CLOTE-CHEQC          PIC S9(7)V USAGE COMP-3.
 39 05     05 P6-E4-QCHEQ-LOTE           PIC S9(9)V USAGE COMP-3.
 44 09     05 P6-E4-VTOT-CHEQC-LOTE      PIC S9(15)V9(2) USAGE COMP-3.
 53 01     05 P6-E4-CINDCD-SELEC-DESC    PIC X(1).
 54 26     05 P6-E4-HULT-ATULZ           PIC X(26).
 80 05     05 P6-E4-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 85 08     05 P6-E4-CTERM                PIC X(8).
 93 10     05 P6-E4-DINCL-REG            PIC X(10).
