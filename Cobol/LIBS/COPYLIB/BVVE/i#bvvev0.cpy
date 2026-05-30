      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  I#BVVEV0 - ARQUIVO RETORNO DO DCOM + RAZAO + SALDO CONTAS     *
      *  TAMANHO: 70                                                   *
      *----------------------------------------------------------------*

       01  BVVEV0-REGISTRO.
           03 BVVEV0-AGENCIA          PIC  9(005).
           03 BVVEV0-CONTA            PIC  9(013).
           03 BVVEV0-VLR-A-DESVINC    PIC S9(015)V9(002) COMP-3.
           03 BVVEV0-GRUPO            PIC  9(003).
           03 BVVEV0-SUBGRUPO         PIC  9(003).
           03 BVVEV0-SALDO-CONTA      PIC S9(015)V9(002) COMP-3.
           03 FILLER                  PIC  X(028).
