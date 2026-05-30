      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOR       *
      * ARQUIVO = MOVIMENTO_INTERFACE       SEQUENCIAL                 *
      * LRECL   = 1026 BYTES                19-06-2007                 *
      *----------------------------------------------------------------*
       01 OR-REG-VAR.
01 02     05 OR-REGISTRO-TAM                PIC  9(004) COMP.
02 598    05 OR-REGISTRO.
 03 24       10 OR-CCHAVE-REG.
 03 04          15 OR-CCTRO-CUSTO           PIC  X(004).
 07 04          15 OR-CEVNTO-ORIGE          PIC  9(004).
 11 08          15 OR-DMOVTO                PIC  9(008).
 19 01          15 OR-CATULZ                PIC  X(001).
 20 01          15 OR-CINDICD-RPROC         PIC  X(001).
 21 06          15 OR-DCOMPT                PIC  9(006).
 27552       10 OR-CHV-ENTID-MOVTO-DESC     PIC  X(552).
579448       10 FILLER                      PIC  X(448).
