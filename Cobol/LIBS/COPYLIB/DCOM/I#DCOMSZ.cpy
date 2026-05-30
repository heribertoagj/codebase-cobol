      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSZ                                      *
      * TAMANHO : 0147 BYTES                                           *
      * ARQUIVO : CONVENIO TAXA DE REDE                                *
      * INCLUDE DA TABELA DCOMB019 - TCONVE_TX_REDE                    *
      ******************************************************************
       01  SZ-19-REGISTRO.
  1  5     05 SZ-19-CCONVE-GRAL-DESC     PIC S9(9)V USAGE COMP-3.
  6  2     05 SZ-19-CVRSAO-CONVE-GRAL    PIC S9(3)V USAGE COMP-3.
  8  1     05 SZ-19-CINDCD-PSSOA-TOMAD   PIC X(1).
  9  2     05 SZ-19-CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
 11  3     05 SZ-19-CRTING               PIC X(3).
 14  2     05 SZ-18-NNVEL-RTING          PIC S9(2)V COMP-3.
 16 26     05 SZ-18-HINIC-VGCIA-NVEL     PIC X(26).
 42  2     05 SZ-19-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
 44 10     05 SZ-19-DINIC-TX-JURO        PIC X(10).
 54  3     05 SZ-19-NSEQ-BANDA-TX-REDE   PIC S9(5)V USAGE COMP-3.
 57 10     05 SZ-19-DFIM-TX-JURO         PIC X(10).
 67  2     05 SZ-19-CINDCD-TX-VLR-MIN    PIC X(2).
 69  9     05 SZ-19-VMIN-FAIXA-OPER      PIC S9(15)V9(2) USAGE COMP-3.
 78  2     05 SZ-19-CINDCD-TX-VLR-MAX    PIC X(2).
 80  9     05 SZ-19-VMAX-FAIXA-OPER      PIC S9(15)V9(2) USAGE COMP-3.
 89  2     05 SZ-19-CINDCD-TX-PRZ-MIN    PIC X(2).
 91  3     05 SZ-19-TMIN-OPER-DESC       PIC S9(5)V USAGE COMP-3.
 94  2     05 SZ-19-CINDCD-TX-PRZ-MAX    PIC X(2).
 96  3     05 SZ-19-TMAX-OPER-DESC       PIC S9(5)V USAGE COMP-3.
 99  4     05 SZ-19-PMIN-TX-REDE-DESC    PIC S9(3)V9(3) USAGE COMP-3.
103  4     05 SZ-19-PMAX-TX-REDE-DESC    PIC S9(3)V9(3) USAGE COMP-3.
107  2     05 SZ-19-CINDCD-FREQ-TX       PIC X(2).
109 26     05 SZ-19-HULT-ATULZ           PIC X(26).
135  5     05 SZ-19-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
140  8     05 SZ-19-CTERM                PIC X(8).
