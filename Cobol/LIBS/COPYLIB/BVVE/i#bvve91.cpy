      **************************************************************
      **                DATA DE CRIACAO = 20/07/2001              **
      **                CODIGO INC. : I#BVVE91                    **
      **                TAMANHO     : 70                          **
      **************************************************************
       01  REG-CARTEIDE.
           02  CARTEID-AGENCIA                  PIC  9(05)    COMP-3.
           02  CARTEID-CONTA                    PIC  9(07)    COMP-3.
           02  CARTEID-DTDEBI                   PIC  9(08).
      **       FORMATO AAAAMMDD
           02  CARTEID-VALOR                    PIC S9(13)V99 COMP-3.
           02  CARTEID-NOMPROP                  PIC  X(35).
           02  CARTEID-NOMAGEN                  PIC  X(12).
