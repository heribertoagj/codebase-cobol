      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  INDENTIFICACAO DE RAV E VINCULO DE SALDO DEVEDOR              *
      *  I#BVVEV5 - ARQUIVO SALDO VINCULADO APOS AMORTIZACAO EMFP      *
      *  TAMANHO: 100                                                  *
      *----------------------------------------------------------------*

       01  BVVEV5-REGISTRO.
           03 BVVEV5-AGENCIA         PIC  9(005).
           03 BVVEV5-CONTA           PIC  9(013).
           03 BVVEV5-BAND            PIC  9(001).
           03 BVVEV5-CREDENC         PIC  X(020).
           03 BVVEV5-VLR-VINC-D-1    PIC  9(015)V9(002) COMP-3.
           03 BVVEV5-VLR-AMORT-EMPF  PIC  9(015)V9(002) COMP-3.
           03 BVVEV5-VLR-SALDO-VINC  PIC  9(015)V9(002) COMP-3.
           03 FILLER                 PIC  X(034).
