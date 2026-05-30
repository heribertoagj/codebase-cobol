      ******************************************************************
      * SISTEMA : DCOM - I#DCOMP3                                      *
      * TAMANHO : 0058 BYTES                                           *
      * ARQUIVO : SIMULACAO CONDICAO DE PAGAMENTO FORNECEDOR           *
      * INCLUDE DA TABELA DCOMB0E1 - DB2PRD.TSMULA_CONDC_PGTO          *
      ******************************************************************
       01  P3-E1-REGISTRO.
01 05      05 P3-E1-NSMULA-OPER-DESC     PIC S9(9)V USAGE COMP-3.
06 03      05 P3-E1-CCONDC-PGTO-FORNC    PIC S9(5)V USAGE COMP-3.
09 01      05 P3-E1-CINDCD-SELEC-DESC    PIC X(1).
10 26      05 P3-E1-HULT-ATULZ           PIC X(26).
36 05      05 P3-E1-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
41 08      05 P3-E1-CTERM                PIC X(8).
49 10      05 P3-E1-DINCL-REG            PIC X(10).
