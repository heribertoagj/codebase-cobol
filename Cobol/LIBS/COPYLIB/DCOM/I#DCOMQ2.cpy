      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQ2                                      *
      * TAMANHO : 0049 BYTES                                           *
      * ARQUIVO : GARANTIA  OPERACAO DESCONTO COMERCIAL                *
      * INCLUDE DA TABELA DCOMB0A6 - DB2PRD.TGARNT_OPER_DESC           *
      ******************************************************************
       01  Q2-A6-REGISTRO.
01 03      05 Q2-A6-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 Q2-A6-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02      05 Q2-A6-CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
11 26      05 Q2-A6-HULT-ATULZ           PIC X(26).
37 05      05 Q2-A6-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
42 08      05 Q2-A6-CTERM                PIC X(8).
