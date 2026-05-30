      ******************************************************************
      * SISTEMA : DCOM - I#DCOMUE                                      *
      * TAMANHO : 0063 BYTES                                           *
      * ARQUIVO : CUSTO EFETIVO TOTAL DA OPERACAO                      *
      * INCLUDE DA TABELA DCOMB0L2 - DB2PRD.TCUSTO_EFETV_TOT           *
      ******************************************************************
       01  UE-L2-REGISTRO.
01 03      05  UE-L2-DANO-OPER-DESC          PIC S9(4)V        COMP-3.
04 05      05  UE-L2-NSEQ-OPER-DESC          PIC S9(9)V        COMP-3.
09 26      05  UE-L2-HATULZ                  PIC X(26).
35 08      05  UE-L2-VCUSTO-DESC-DIA         PIC S9(8)V9(7)    COMP-3.
43 08      05  UE-L2-VCUSTO-DESC-MES         PIC S9(8)V9(7)    COMP-3.
51 08      05  UE-L2-VCUSTO-DESC-ANO         PIC S9(8)V9(7)    COMP-3.
59 05      05  UE-L2-CFUNC-BDSCO             PIC S9(9)V        COMP-3.
