      *****************************************************************
      *                                                               *
      *  I#BVVEF4                                                     *
      *                                                               *
      *  BVVE - ARQUIVO TEMPORARIO PARA MONTAGEM DE FITA4 - AMEX      *
      *         RESUMO DOS ARQUIVOS TF E EBTA                         *
      *                                                               *
      *  LRECL= 050 BYTES                                             *
      *  CRIACAO: MAIO/2013                                           *
      *                                                               *
      *****************************************************************

       01  REG-TMP-F4.
           03  TMP-F4-AGENCIA                         PIC  9(005).
           03  TMP-F4-CONTA                           PIC  9(007).
           03  TMP-F4-DCTA                            PIC  X(001).
           03  TMP-F4-SINAL                           PIC  X(001).
           03  TMP-F4-VALOR                           PIC  9(015)V99.
           03  FILLER                                 PIC  X(019).

