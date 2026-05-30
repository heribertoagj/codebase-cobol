      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPA                                      *
      * TAMANHO : 0070 BYTES                                           *
      * ARQUIVO : SIMULACAO CONTA CHEQUE                               *
      * INCLUDE DA TABELA DCOMB0H6 - DB2PRD.TSMULA_CTA_CHEQC           *
      ******************************************************************
       01  PA-H6-REGISTRO.
 01 05     05 PA-H6-NSMULA-OPER-DESC     PIC S9(9)V USAGE COMP-3.
 06 02     05 PA-H6-CBCO-CLI-CHEQC       PIC S9(3)V USAGE COMP-3.
 08 03     05 PA-H6-CDEPDC               PIC S9(5)V USAGE COMP-3.
 11 07     05 PA-H6-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
 18 03     05 PA-H6-CFLIAL-EMPR-CHEQC    PIC S9(5)V USAGE COMP-3.
 21 01     05 PA-H6-CINDCD-SELEC-DESC    PIC X(1).
 22 26     05 PA-H6-HULT-ATULZ           PIC X(26).
 48 05     05 PA-H6-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 53 08     05 PA-H6-CTERM                PIC X(8).
 61 10     05 PA-H6-DINCL-REG            PIC X(10).
