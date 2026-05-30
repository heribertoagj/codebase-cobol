      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *  INDENTIFICACAO DE RAV E VINCULO DE SALDO DEVEDOR              *
      *  I#BVVEV7 - ARQUIVO RECEBIDO DO EMPF - AMORTIZACOES COM BANDEIRA
      *  TAMANHO: 60                                                   *
      *----------------------------------------------------------------*

       01  BVVEV7-REGISTRO.
           03 BVVEV7-AGENCIA       PIC  9(005).
           03 BVVEV7-CONTA         PIC  9(013).
WI1120     03 BVVEV7-BAND          PIC  9(003).
           03 BVVEV7-CREDENC       PIC  X(020).
           03 BVVEV7-VALOR         PIC  9(015)V9(002) COMP-3.
           03 FILLER               PIC  X(010).
