      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO                            *
      *  ARQUIVO : ARQUIVO DE DATAS DO SISTEMA                         *
      *  LRECL   : 050 (FB)                                            *
      *  NOME INC: I#BVVENY                                            *
      *  DATA    : 10/2020                                             *
      *----------------------------------------------------------------*
       01  BVVENY-REG.
           10 BVVENY-DT-MOVTO-INV         PIC  9(008).
           10 BVVENY-DT-MOVTO-DB2         PIC  X(010).
           10 BVVENY-DT-MOVTO             PIC  9(008).
           10 BVVENY-DT-MOVTO-PROX        PIC  9(008).
           10 BVVENY-DT-MOVTO-ANT         PIC  9(008).
           10 FILLER                      PIC  X(008).
