      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  INDENTIFICACAO DE RAV E VINCULO DE SALDO DEVEDOR              *
      *  I#BVVEV1 - PRV BANDEIRA - VALOR VINCULADO DIARIO POR BANDEIRA *
      *  TAMANHO: 100                                                  *
      *----------------------------------------------------------------*

       01  BVVEV1-REGISTRO.
           03 BVVEV1-AGENCIA        PIC  9(005).
           03 BVVEV1-CONTA          PIC  9(013).
           03 BVVEV1-CTPO-VISAO-BANDE
                                    PIC  9(003) COMP-3.
           03 BVVEV1-CBANDE-VISAO   PIC  9(003) COMP-3.
           03 BVVEV1-VLR-COMPROMET  PIC  9(015)V9(002) COMP-3.
           03 BVVEV1-VLR-VINC-D0    PIC  9(015)V9(002) COMP-3.
           03 BVVEV1-VLR-VINC-D-1   PIC  9(015)V9(002) COMP-3.
           03 BVVEV1-VLR-A-VINCULAR PIC  9(015)V9(002) COMP-3.
           03 BVVEV1-DT-MOVTO       PIC  9(008).
           03 FILLER                PIC  X(034).
