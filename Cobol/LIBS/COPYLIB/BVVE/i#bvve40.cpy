      *================================================================*
      *  I#BVVE40 - IMAGEM DA TABELA BACCV040                          *
      *             BASE DE LIMITES DE ANTECIPACOES                    *
      *----------------------------------------------------------------*
      *  LRECL: 062                                                    *
      *================================================================*
       01  REG-BACCV040.
           05  BACCV040-AGENCIA            PIC S9(005)     COMP-3.
           05  BACCV040-CONTA              PIC S9(007)     COMP-3.
           05  BACCV040-PRODUTO            PIC S9(005)     COMP-3.
           05  BACCV040-PERC-LIM           PIC S9(003)V99  COMP-3.
           05  BACCV040-VMOVTO-MAX         PIC S9(013)V99  COMP-3.
           05  BACCV040-VTOT-CREDT         PIC S9(013)V99  COMP-3.
           05  BACCV040-VTOT-DEBT          PIC S9(013)V99  COMP-3.
           05  BACCV040-VDISP-ANTEC        PIC S9(013)V99  COMP-3.
           05  BACCV040-VTOT-MOVTO-ANTEC   PIC S9(013)V99  COMP-3.
           05  BACCV040-VTOT-MOVTO-CREDT   PIC S9(013)V99  COMP-3.
           05  BACCV040-VTOT-MOVTO-CREDT-N PIC  X(001).
