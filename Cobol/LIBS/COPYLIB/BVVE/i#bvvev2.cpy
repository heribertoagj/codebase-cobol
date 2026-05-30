      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  INDENTIFICACAO DE RAV E VINCULO DE SALDO DEVEDOR              *
      *  I#BVVEV2 - PRV RAV - VALOR RAVS E VINCULADO DIARIO            *
      *  TAMANHO: 150                                                  *
      *----------------------------------------------------------------*

       01  BVVEV2-REGISTRO.
           03 BVVEV2-AGENCIA          PIC  9(005).
           03 BVVEV2-CONTA            PIC  9(013).
           03 BVVEV2-CTPO-VISAO-BANDE
                                      PIC  9(003) COMP-3.
           03 BVVEV2-CBANDE-VISAO     PIC  9(003) COMP-3.
           03 BVVEV2-NOME-CRED        PIC  X(020).
           03 BVVEV2-VLR-RAV-CICLO    PIC  9(015)V9(002) COMP-3.
           03 BVVEV2-VLR-RAV-ACUM-D0  PIC  9(015)V9(002) COMP-3.
           03 BVVEV2-VLR-RAV-ACUM-D-1 PIC  9(015)V9(002) COMP-3.
           03 BVVEV2-VLR-VINC-D0      PIC  9(015)V9(002) COMP-3.
           03 BVVEV2-VLR-VINC-D-1     PIC  9(015)V9(002) COMP-3.
           03 BVVEV2-DT-MOVTO         PIC  9(008).
           03 FILLER                  PIC  X(055).
