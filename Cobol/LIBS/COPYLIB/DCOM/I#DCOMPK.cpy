      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPK                                      *
      * TAMANHO : 0049 BYTES                                           *
      * ARQUIVO : TRANSITORIA GARANTIA OPERACAO DESCONTO               *
      * INCLUDE DA TABELA DCOMB0G1 - DB2PRD.TTRASI_GARNT_OPER          *
      ******************************************************************
       01  PK-G1-REGISTRO.
01 03      05 PK-G1-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 PK-G1-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02      05 PK-G1-CTPO-GARNT           PIC S9(3)V USAGE COMP-3.
11 26      05 PK-G1-HULT-ATULZ           PIC X(26).
37 05      05 PK-G1-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
42 08      05 PK-G1-CTERM                PIC X(8).
