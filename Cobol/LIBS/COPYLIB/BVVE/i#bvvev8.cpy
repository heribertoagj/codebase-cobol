      *----------------------------------------------------------------*
      *  CONSTITUICAO DE GARANTIA DE ORPAG'S POR SALDO DEVEDOR         *
      *                                                                *
      *  I#BVVEV8 - AGENCIA/CONTA MOVIMENTO E VINCULADA UTILIZADAS P/  *
      *             DEBITO E CREDITO NOS MODULOS COSA                  *
      *  TAMANHO: 120                                                  *
      *----------------------------------------------------------------*

       01  BVVEV8-REGISTRO.
           03 BVVEV8-DADOS-CTA-MOVTO.
               05 BVVEV8-MOVTO-AGEN        PIC  9(005).
               05 BVVEV8-MOVTO-CONTA       PIC  9(013).
               05 BVVEV8-MOVTO-DIG-CTA     PIC  X(001).
               05 BVVEV8-MOVTO-RAZAO       PIC  9(003).
               05 BVVEV8-MOVTO-SRAZAO      PIC  9(003).
           03 BVVEV8-DADOS-CTA-VINCL.
               05 BVVEV8-VINCL-AGEN        PIC  9(005).
               05 BVVEV8-VINCL-CONTA       PIC  9(013).
               05 BVVEV8-VINCL-DIG-CTA     PIC  X(001).
               05 BVVEV8-VINCL-RAZAO       PIC  9(003).
               05 BVVEV8-VINCL-SRAZAO      PIC  9(003).
           03 BVVEV8-VALOR                 PIC  9(015)V9(002) COMP-3.
           03 BVVEV8-SEQUENCIAL            PIC  9(009) COMP-3.
           03 BVVEV8-RESERVA               PIC  X(056).
