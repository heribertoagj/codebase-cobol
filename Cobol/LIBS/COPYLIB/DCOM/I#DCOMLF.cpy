      *****************************************************************
      * SISTEMA...: DCOM - DESCONTO COMERCIAL        NOME: I#DCOMLF   *
      * DESCRICAO.: INTERFACE DCOM X TRIC                             *
      * TAMANHO...: LRECL = 200                                       *
      *****************************************************************
      *
       01  DCOMLF-TRIC-REG.
           05  DCOMLF-BCO              PIC  9(003).
           05  DCOMLF-AGC              PIC  9(005).
           05  DCOMLF-CTA              PIC  9(007).
           05  DCOMLF-DTA              PIC  X(010).
           05  DCOMLF-PRODT            PIC  9(003).
           05  DCOMLF-SPROD            PIC  9(003).
           05  DCOMLF-FATGER           PIC  9(003).
           05  DCOMLF-TPPESS           PIC  X(001).
           05  DCOMLF-MEENT            PIC  9(003).
           05  DCOMLF-SITDES           PIC  9(003).
           05  DCOMLF-VALOR-1          PIC  9(013)V99 COMP-3.
           05  DCOMLF-VALOR-2          PIC  9(013)V99 COMP-3.
           05  DCOMLF-VALOR-3          PIC  9(013)V99 COMP-3.
           05  DCOMLF-VALOR-4          PIC  9(013)V99 COMP-3.
           05  DCOMLF-VALOR-5          PIC  9(013)V99 COMP-3.
           05  DCOMLF-CONTCOR          PIC  9(007).
           05  DCOMLF-STR              PIC  X(003).
           05  DCOMLF-BCO-DST          PIC  9(003).
           05  DCOMLF-AGC-DST          PIC  9(005).
           05  DCOMLF-CTA-DST          PIC  9(007).
           05  DCOMLF-HIST-COML        PIC  X(032).
           05  DCOMLF-CTPO-DESC-COML   PIC  S9(3)V USAGE COMP-3.
           05  FILLER                  PIC  X(060).
