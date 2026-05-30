      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSN                                      *
      * TAMANHO : 0147 BYTES                                           *
      * ARQUIVO : CONVENIO CLIENTE TAXA DE REDE                        *
      * INCLUDE DA TABELA DCOMB014 - TCONVE_CLI_TX_REDE                *
      ******************************************************************
       01  SN-14-REGISTRO.
  1  5     05 SN-14-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SN-14-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  1     05 SN-14-CINDCD-PSSOA-TOMAD   PIC X(1).
  9  2     05 SN-14-CSGMTO-CLI           PIC S9(3)V USAGE COMP-3.
 11  3     05 SN-14-CRTING               PIC X(3).
 14  2     05 SN-14-NNVEL-RTING          PIC S9(2)V COMP-3.
 16 26     05 SN-14-HINIC-VGCIA-NVEL     PIC X(26).
 42  2     05 SN-14-CMEIO-ENTRD-DESC     PIC S9(3)V USAGE COMP-3.
 44 10     05 SN-14-DINIC-TX-JURO        PIC X(10).
 54  3     05 SN-14-NSEQ-BANDA-TX-REDE   PIC S9(5)V USAGE COMP-3.
 57 10     05 SN-14-DFIM-TX-JURO         PIC X(10).
 67  2     05 SN-14-CINDCD-TX-VLR-MIN    PIC X(2).
 69  9     05 SN-14-VMIN-FAIXA-OPER      PIC S9(15)V9(2) USAGE COMP-3.
 78  2     05 SN-14-CINDCD-TX-VLR-MAX    PIC X(2).
 80  9     05 SN-14-VMAX-FAIXA-OPER      PIC S9(15)V9(2) USAGE COMP-3.
 89  2     05 SN-14-CINDCD-TX-PRZ-MIN    PIC X(2).
 91  3     05 SN-14-TMIN-OPER-DESC       PIC S9(5)V USAGE COMP-3.
 94  2     05 SN-14-CINDCD-TX-PRZ-MAX    PIC X(2).
 96  3     05 SN-14-TMAX-OPER-DESC       PIC S9(5)V USAGE COMP-3.
 99  4     05 SN-14-PMIN-TX-REDE-DESC    PIC S9(3)V9(3) USAGE COMP-3.
103  4     05 SN-14-PMAX-TX-REDE-DESC    PIC S9(3)V9(3) USAGE COMP-3.
107  2     05 SN-14-CINDCD-FREQ-TX       PIC X(2).
109 26     05 SN-14-HULT-ATULZ           PIC X(26).
135  5     05 SN-14-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
140  8     05 SN-14-CTERM                PIC X(8).
