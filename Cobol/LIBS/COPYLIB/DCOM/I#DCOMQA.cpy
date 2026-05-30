      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQA                                      *
      * TAMANHO : 0052 BYTES                                           *
      * ARQUIVO : OPERACARO ROTEIRO DE INCLUSAO                        *
      * INCLUDE DA TABELA DCOMB0C8 - DB2PRD.TOPER_ROTRO_INCLI          *
      ******************************************************************
       01  QA-C8-REGISTRO.
01 03      05 QA-C8-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 QA-C8-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02      05 QA-C8-CTPO-AGPTO-ROTRO     PIC S9(3)V USAGE COMP-3.
11 01      05 QA-C8-CINDCD-OBRIG-AGPTO   PIC X(1).
12 01      05 QA-C8-CINDCD-SIT-AGPTO     PIC X(1).
13 01      05 QA-C8-CINDCD-CONCL-ROTRO   PIC X(1).
14 26      05 QA-C8-HULT-ATULZ           PIC X(26).
40 05      05 QA-C8-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
45 08      05 QA-C8-CTERM                PIC X(8).
