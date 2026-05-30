      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  I#BVVEVC - RELATORIO DESMONTE DE CONTA GARANTIA               *
      *  TAMANHO: 110                                                  *
      *----------------------------------------------------------------*

       01  BVVEVC-REGISTRO.
           03 BVVEVC-RET-DCOM.
              05 BVVEVC-AGENCIA          PIC  9(005).
              05 BVVEVC-CONTA            PIC  9(013).
              05 BVVEVC-VLR-A-DESVINC    PIC S9(015)V9(002) COMP-3.
              05 BVVEVC-SALDO-CONTA      PIC S9(015)V9(002) COMP-3.
           03 BVVEVC-ARQ-GRAN.
              05 BVVEVC-AGENCIA-GRAN     PIC  9(005).
              05 BVVEVC-CONTA-GRAN       PIC  9(013).
              05 BVVEVC-SALDO-CONTA-GRAN PIC S9(015)V9(002) COMP-3.
              05 BVVEVC-VLR-TRANSF       PIC S9(015)V9(002) COMP-3.
           03 BVVEVC-VLR-SOBRA           PIC S9(015)V9(002) COMP-3.
           03 FILLER                     PIC  X(029).
