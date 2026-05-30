      *==============================================================*
      *   BOOK -  I#BVVERF                                           *
      *           CREDITOS FUTUROS                                   *
      ****************************************************************
      *   TAMANHO        0050  BYTES                                 *
      ****************************************************************
       01  REG-CREFUTF.
           05  CREFUTF-AGEN            PIC  9(05)     COMP-3.
           05  CREFUTF-CONTA           PIC  9(07)     COMP-3.
           05  CREFUTF-DTCRED.
               10  CREFUTF-AACRED      PIC  9(04).
               10  CREFUTF-MMCRED      PIC  9(02).
               10  CREFUTF-DDCRED      PIC  9(02).
           05  CREFUTF-MOEDA           PIC  X(01).
           05  CREFUTF-TIPOEXT         PIC  X(01).
           05  CREFUTF-NUMVEND         PIC  9(13)     COMP-3.
           05  CREFUTF-VALOR           PIC S9(13)V99  COMP-3.
           05  FILLER                  PIC  X(18).
      *
