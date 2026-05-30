      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSM                                      *
      * TAMANHO : 0112 BYTES                                           *
      * ARQUIVO : CLIENTE TAXA DEFINIDA                                *
      * INCLUDE DA TABELA DCOMB009 - TCLI_TX_DFNDO                     *
      ******************************************************************
       01  SM-09-REGISTRO.
  1  5     05 SM-09-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SM-09-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  1     05 SM-09-CINDCD-PSSOA-TOMAD   PIC X(1).
  9  2     05 SM-09-CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
 11  3     05 SM-09-CRTING               PIC X(3).
 14  2     05 SM-09-NNVEL-RTING          PIC S9(2)V COMP-3.
 16 26     05 SM-09-HINIC-VGCIA-NVEL     PIC X(26).
 42  2     05 SM-09-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
 44 10     05 SM-09-DINIC-TX-JURO        PIC X(10).
 54 10     05 SM-09-DFIM-TX-JURO         PIC X(10).
 64  5     05 SM-09-PTX-JURO-DFNDO       PIC S9(3)V9(5) USAGE COMP-3.
 69  2     05 SM-09-CINDCD-FREQ-TX       PIC X(2).
 71  3     05 SM-09-CIDTFD-TX-DFNDO      PIC S9(5)V USAGE COMP-3.
 74 26     05 SM-09-HULT-ATULZ           PIC X(26).
100  5     05 SM-09-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
105  8     05 SM-09-CTERM                PIC X(8).
