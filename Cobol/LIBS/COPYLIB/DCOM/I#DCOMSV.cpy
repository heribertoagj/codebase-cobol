      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSV                                      *
      * TAMANHO : 0105 BYTES                                           *
      * ARQUIVO : CONVENIO GERAL CRITERIO DE REDE                      *
      * INCLUDE DA TABELA DCOMB0A7 - TGRAL_CRITR_REDE                  *
      ******************************************************************
       01  SV-A7-REGISTRO.
  1  5     05 SV-A7-CCONVE-GRAL-DESC     PIC S9(9)V USAGE COMP-3.
  6  2     05 SV-A7-CVRSAO-CONVE-GRAL    PIC S9(3)V USAGE COMP-3.
  8  1     05 SV-A7-CINDCD-PSSOA-TOMAD   PIC X(1).
  9  2     05 SV-A7-CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
 11  3     05 SV-A7-CRTING               PIC X(3).
 14  2     05 SV-A7-NNVEL-RTING          PIC S9(2)V COMP-3.
 16 26     05 SV-A7-HINIC-VGCIA-NVEL     PIC X(26).
 42  2     05 SV-A7-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
 44 10     05 SV-A7-DINIC-CRITR-REDE     PIC X(10).
 54 10     05 SV-A7-DFIM-CRITR-TX-REDE   PIC X(10).
 64  3     05 SV-A7-CIDTFD-TX-MAX-REDE   PIC S9(5)V USAGE COMP-3.
 67  8     05 SV-A7-CTERM                PIC X(8).
 75  5     05 SV-A7-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 80 26     05 SV-A7-HULT-ATULZ           PIC X(26).
