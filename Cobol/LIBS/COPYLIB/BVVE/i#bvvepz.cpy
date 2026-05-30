      ******************************************************************
      *    I#BVVEPZ - LAY-OUT DO ARQUIVO DE PRAZO DE PAGAMENTO         *
      *    LRECL: 50                                                   *
      ******************************************************************
       01  PARM-PRAZO.
           03  PARM-PRAZ-CH.
               05  PARM-PRAZ-CH-CODIGO-PROD  PIC 9(03).
           03  PARM-PRAZ-MAXIMO-DIAS         PIC 9(04).
           03  PARM-PRAZ-DT-INCLUSAO         PIC 9(08).
           03  PARM-PRAZ-USUARIO             PIC X(09).
           03  FILLER                        PIC X(26).
