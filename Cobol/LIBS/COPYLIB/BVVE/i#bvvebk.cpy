      *================================================================*
      * BOOK I#BVVEBK - IMAGEM DA BASE BACCV040 + BANDEIRA             *
      * TAMANHO : 080                                                  *
      *================================================================*
      *                                                                *
       01  BVVEBK-REGISTRO.
           05 BVVEBK-AGENCIA               PIC S9(005)     COMP-3.
           05 BVVEBK-CONTA                 PIC S9(007)     COMP-3.
           05 BVVEBK-PRODUTO               PIC S9(005)     COMP-3.
           05 BVVEBK-PERC-LIM              PIC S9(003)V99  COMP-3.
           05 BVVEBK-VMOVTO-MAX            PIC S9(013)V99  COMP-3.
           05 BVVEBK-VTOT-CREDT            PIC S9(013)V99  COMP-3.
           05 BVVEBK-VTOT-DEBT             PIC S9(013)V99  COMP-3.
           05 BVVEBK-VDISP-ANTEC           PIC S9(013)V99  COMP-3.
           05 BVVEBK-VTOT-MOVTO-ANTEC      PIC S9(013)V99  COMP-3.
           05 BVVEBK-VTOT-MOVTO-CREDT      PIC S9(013)V99  COMP-3.
           05 BVVEBK-N-VTOT-MOVTO-CREDT    PIC  X(001).
WI1120     05 BVVEBK-COD-BANDEIRA          PIC  9(003).
           05 BVVEBK-RESERVA               PIC  X(015).
      *                                                                *
      *================================================================*
