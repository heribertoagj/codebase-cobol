      ******************************************************************
      *    I#BVVEBN - LAY-OUT DO ARQUIVO DE BENEFICIOS MINIMO E MAXIMO *
      *    LRECL: 100                                                  *
      ******************************************************************
       01  PARM-BENEFICIO-MIN-MAX.
           03  PARM-BENE-CH.
               05  PARM-BENE-CH-CODIGO-PROD  PIC 9(03).
           03  PARM-BENE-VALOR-MIN           PIC 9(05)V99.
           03  PARM-BENE-VALOR-MAX           PIC 9(05)V99.
           03  PARM-BENE-DT-INCLUSAO         PIC 9(08).
           03  PARM-BENE-USUARIO             PIC X(09).
           03  FILLER                        PIC X(66).
