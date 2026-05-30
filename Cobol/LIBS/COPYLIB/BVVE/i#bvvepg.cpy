      *----------------------------------------------------------------*
      * BOOK I#BVVEPG     MOVTO VENDEDOR - FATURA FECHADA              *
      *                   PAGAMENTO   DIARIO                           *
      * TAMANHO : 145                                                  *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01         MVPG-REG.
      *                                                        001  145
           05     MVPG-AGENC         PIC  9(05)  COMP-3.
      *                                                        001  003
           05     MVPG-CONTA         PIC  9(07)  COMP-3.
      *                                                        004  004
           05     MVPG-NUM-VEND      PIC  9(13)  COMP-3.
      *                                                        008  007
           05     MVPG-DTFECH        PIC  X(10).
      *                FORMATO DD.MM.AAAA                      015  010
      *
           05     MVPG-DTCRED        PIC  X(10).
      *                FORMATO DD.MM.AAAA                      025  010
           05     MVPG-TIP-REG       PIC  9(03)  COMP-3.
      *                100 - COMPRAS                           035  002
      *                150 - COMPRAS
      *                200 - SALDO ANTERIOR
      *                300 - TOTAL DEVOLUCOES
      *                400 - COMISSAO
      *                500 - DETALHE DEVOLUCAO
      *
           05     MVPG-NUM-REMES     PIC  9(07)  COMP-3.
      *                                                        037  004
           05     MVPG-DTREMES       PIC  X(10).
      *                 FORMATO DD.MM.AAAA                     041  010
      *
           05     MVPG-DTCRED-REMES  PIC  X(10).
      *                 FORMATO DD.MM.AAAA                     051  010
      *
           05     MVPG-VALOR         PIC  9(11)V99  COMP-3.
      *                                                        061  007
           05     MVPG-TIP-CRED      PIC  X(01).
      *                                                        068  001
      *                                                        069  004
           05     MVPG-NUM-REMES-OR  PIC  9(07)  COMP-3.
      *                                                        073  004
           05     MVPG-NUM-PORT-ORIG PIC  X(19).
      *                                                        077  019
           05     MVPG-COD-TRAN      PIC  X(03).
      *                INC - INCLUSAO                          096  003
      *                ALT - ALTERACAO
      *                EXC - EXCLUSAO
      *
           05     MVPG-DES-LAYOUT    PIC  X(47).
      *                                                        099  047
