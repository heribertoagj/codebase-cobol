      ******************************************************************
      * SISTEMA : DCOM - I#DCOMUF                                      *
      * TAMANHO : 0060 BYTES                                           *
      * ARQUIVO : CUSTO EFETIVO TOTAL DA SIMULCAO                      *
      * INCLUDE DA TABELA DCOMB0L4 - DB2PRD.TSMULA_CUSTO_EFETV         *
      ******************************************************************
       01  UF-L4-REGISTRO.
01 05      05 UF-L4-NSMULA-OPER-DESC         PIC S9(9)V        COMP-3.
06 08      05 UF-L4-VCUSTO-DESC-DIA          PIC S9(8)V9(7)    COMP-3.
14 08      05 UF-L4-VCUSTO-DESC-MES          PIC S9(8)V9(7)    COMP-3.
22 08      05 UF-L4-VCUSTO-DESC-ANO          PIC S9(8)V9(7)    COMP-3.
30 05      05 UF-L4-CFUNC-BDSCO              PIC S9(9)V        COMP-3.
35 26      05 UF-L4-HATULZ                   PIC X(26).
