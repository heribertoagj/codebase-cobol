      *================================================================*
      * I#BVVETH - TRAVA AUTOMATICA                                    *
      *            BASE COMPLETA DE DOMICILIOS BRADESCO                *
      *----------------------------------------------------------------*
      * LRECL: 420 / CHAVE: 115                                        *
      *================================================================*
       01  BVVETH-REGISTRO.
           05 BVVETH-CHAVE.
              10 BVVETH-CH-CNPJ-CPF      PIC  9(015).
      *          CPF  = 999999999990000 => 999999999-99 0000
      *          CNPJ = 099999999999999 => 099999999/9999-99
              10 BVVETH-CH-BAND-CIP      PIC  X(003).
      *          003 = MASTER
      *          004 = VISA
              10 BVVETH-CH-AGENCIA       PIC  9(005).
              10 BVVETH-CH-CONTA         PIC  9(013).
              10 BVVETH-CH-RESERVA       PIC  X(079).
           05 BVVETH-IDENT-ORIGEM        PIC  X(002).
      *       AC = AGENDA CIP
      *       TC = TDB CIELO
      *       CC = CADASTRO CIELO MENSAL
           05 BVVETH-HATULZ              PIC  X(026).
           05 BVVETH-MES-REFER           PIC  9(002).
           05 BVVETH-TP-PSSOA            PIC  X(001).
      *       F = PESSOA FISICA
      *       J = PESSOA JURIDICA
           05 BVVETH-RESERVA             PIC  X(274).
