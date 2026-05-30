      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSW                                      *
      * TAMANHO : 0112 BYTES                                           *
      * ARQUIVO : CONVENIO TAXA DEFINIDA                               *
      * INCLUDE DA TABELA DCOMB018 - TCONVE_TX_DFNDO                   *
      ******************************************************************
       01  SW-18-REGISTRO.
  1  5     05 SW-18-CCONVE-GRAL-DESC     PIC S9(9)V USAGE COMP-3.
  6  2     05 SW-18-CVRSAO-CONVE-GRAL    PIC S9(3)V USAGE COMP-3.
  8  1     05 SW-18-CINDCD-PSSOA-TOMAD   PIC X(1).
  9  2     05 SW-18-CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
 11  3     05 SW-18-CRTING               PIC X(3).
 14  2     05 SW-18-NNVEL-RTING          PIC S9(2)V COMP-3.
 16 26     05 SW-18-HINIC-VGCIA-NVEL     PIC X(26).
 42  2     05 SW-18-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
 44 10     05 SW-18-DINIC-TX-JURO        PIC X(10).
 54 10     05 SW-18-DFIM-TX-JURO         PIC X(10).
 64  5     05 SW-18-PTX-JURO-DFNDO       PIC S9(3)V9(5) USAGE COMP-3.
 69  2     05 SW-18-CINDCD-FREQ-TX       PIC X(2).
 71  3     05 SW-18-CIDTFD-TX-DFNDO      PIC S9(5)V USAGE COMP-3.
 74 26     05 SW-18-HULT-ATULZ           PIC X(26).
100  5     05 SW-18-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
105  8     05 SW-18C-TERM                PIC X(8).
