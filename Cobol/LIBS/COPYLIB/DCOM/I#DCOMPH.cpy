      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPH                                      *
      * TAMANHO : 0061 BYTES                                           *
      * ARQUIVO : TRANSITORIA CONDICAO PAGAMENTO FORNECEDOR            *
      * INCLUDE DA TABELA DCOMB0F7 - DB2PRD.TTRASI_CONDC_PGTO          *
      ******************************************************************
       01  PH-F7-REGISTRO.
01  03     05 PH-F7-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04  05     05 PH-F7-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09  03     05 PH-F7-CCONDC-PGTO-FORNC    PIC S9(5)V USAGE COMP-3.
12  01     05 PH-F7-CINDCD-SELEC-DESC    PIC X(1).
13  26     05 PH-F7-HULT-ATULZ           PIC X(26).
39  05     05 PH-F7-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
44  08     05 PH-F7-CTERM                PIC X(8).
52  10     05 PH-F7-DINCL-REG            PIC X(10).
