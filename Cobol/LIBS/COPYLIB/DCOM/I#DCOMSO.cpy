      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSO                                      *
      * TAMANHO : 0105 BYTES                                           *
      * ARQUIVO : CLIENTE CRITERIO DE REDE                             *
      * INCLUDE DA TABELA DCOMB096 - TCLI_CRITERIO_REDE                *
      ******************************************************************
       01  SO-96-REGISTRO.
  1  5     05 SO-96-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SO-96-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  1     05 SO-96-CINDCD-PSSOA-TOMAD   PIC X(1).
  9  2     05 SO-96-CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
 11  3     05 SO-96-CRTING               PIC X(3).
 14  2     05 SO-14-NNVEL-RTING          PIC S9(2)V COMP-3.
 16 26     05 SO-14-HINIC-VGCIA-NVEL     PIC X(26).
 42  2     05 SO-96-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
 44 10     05 SO-96-DINIC-CRITR-REDE     PIC X(10).
 54 10     05 SO-96-DFIM-CRITR-TX-REDE   PIC X(10).
 64  3     05 SO-96-CIDTFD-TX-MAX-REDE   PIC S9(5)V USAGE COMP-3.
 67  8     05 SO-96-CTERM                PIC X(8).
 75  5     05 SO-96-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 80 26     05 SO-96-HULT-ATULZ           PIC X(26).
