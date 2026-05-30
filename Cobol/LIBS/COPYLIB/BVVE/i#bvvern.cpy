      *==============================================================*
      *   BOOK -  I#BVVERN                                           *
      *           CREDITOS FUTUROS                                   *
      ****************************************************************
      *   TAMANHO        0070  BYTES                                 *
      ****************************************************************
       01  REG-FUTR.
           05  FUTR-AGEN              PIC  9(05)     COMP-3.
           05  FUTR-CONTA             PIC  9(07)     COMP-3.
           05  FUTR-PRODUTO           PIC  9(05)     COMP-3.
           05  FUTR-DTCRED.
               10  FUTR-AACRED        PIC  9(04).
               10  FUTR-MMCRED        PIC  9(02).
               10  FUTR-DDCRED        PIC  9(02).
           05  FUTR-MOEDA             PIC  X(01).
           05  FUTR-TIPOEXP           PIC  X(01).
           05  FUTR-NUMVEND           PIC  9(13)     COMP-3.
           05  FUTR-VALOR             PIC S9(13)V99  COMP-3.
           05  FUTR-NOME              PIC  X(35).
