      *----------------------------------------------------------------*
      * BOOK I#BVVEFF     MOVTO VENDEDOR - FATURA FECHADA              *
      *                   PAGAMENTO   DIARIO                           *
      * TAMANHO : 145                                                  *
      *----------------------------------------------------------------*
      *                                                   POS.INIC  TAM.
       01         FATF-REG.
      *                                                        001  145
           05     FATF-AGENC         PIC  9(05)  COMP-3.
      *                                                        001  003
           05     FATF-CONTA         PIC  9(07)  COMP-3.
      *                                                        004  004
           05     FATF-NUM-VEND      PIC  9(13)  COMP-3.
      *                                                        008  007
           05     FATF-DTFECH        PIC  X(10).
      *                FORMATO AAAA-MM-DD                      015  010
      *
           05     FATF-DTCRED        PIC  X(10).
      *                FORMATO AAAA-MM-DD                      025  010
           05     FATF-TIP-REG       PIC  9(03)  COMP-3.
      *                100 - COMPRAS                           035  002
      *                150 - COMPRAS
      *                200 - SALDO ANTERIOR
      *                300 - TOTAL DEVOLUCOES
      *                400 - COMISSAO
      *                500 - DETALHE DEVOLUCAO
      *
           05     FATF-NUM-REMES     PIC  9(07)  COMP-3.
      *                                                        037  004
           05     FATF-DTREMES       PIC  X(10).
      *                 FORMATO AAAA-MM-DD                     041  010
      *
           05     FATF-DTCRED-REMES  PIC  X(10).
      *                 FORMATO AAAA-MM-DD                     051  010
      *
           05     FATF-VALOR         PIC  9(11)V99  COMP-3.
      *                                                        061  007
           05     FATF-TIP-CRED      PIC  X(01).
      *                                                        068  001
      *                                                        069  004
           05     FATF-NUM-REMES-OR  PIC  9(07)  COMP-3.
      *                                                        073  004
           05     FATF-NUM-PORT-ORIG PIC  X(19).
      *                                                        077  019
           05     FATF-COD-LAYOUT    PIC  X(03).
      *                                                        096  003
           05     FATF-DES-LAYOUT    PIC  X(47).
      *                                                        099  047
