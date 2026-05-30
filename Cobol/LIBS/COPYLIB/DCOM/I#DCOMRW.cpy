      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRW                                      *
      * TAMANHO : 0073 BYTES                                           *
      * ARQUIVO : CONTRATO CONS CREDITO                                *
      * INCLUDE DA TABELA DCOMB0H0 - TCONTR_CONS_CREDT                 *
      ******************************************************************
       01  RW-H0-REGISTRO.
  1  5     10 RW-H0-CCONTR-LIM-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     10 RW-H0-CVRSAO-CONTR-LIM     PIC S9(3)V USAGE COMP-3.
  8  2     10 RW-H0-NSEQ-CONS-CREDT      PIC S9(3)V USAGE COMP-3.
 10  3     10 RW-H0-DANO-BASE            PIC S9(4)V USAGE COMP-3.
 13  3     10 RW-H0-CDEPDC               PIC S9(5)V USAGE COMP-3.
 16  5     10 RW-H0-CSEQ-STUDO           PIC S9(9)V USAGE COMP-3.
 21  9     10 RW-H0-VCONS-NEGOC          PIC S9(15)V9(2) USAGE COMP-3.
 30  5     10 RW-H0-CCART-LIM-CREDT      PIC X(5).
 35 26     10 RW-H0-HULT-ATULZ           PIC X(26).
 61  5     10 RW-H0-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 66  8     10 RW-H0-CTERM                PIC X(8).
