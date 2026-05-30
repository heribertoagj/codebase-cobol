      ******************************************************************
      *    I#BVVETF - LAY-OUT DO ARQUIVO DE TARIFAS                    *
      *    LRECL: 100                                                  *
      ******************************************************************
       01  PARM-TARIFA.
           03  PARM-TARI-CH.
               05  PARM-TARI-CH-CODIGO       PIC 9(03).
           03  PARM-TARI-DESCRICAO           PIC X(40).
           03  PARM-TARI-DT-INCLUSAO         PIC 9(08).
           03  PARM-TARI-USUARIO             PIC X(09).
           03  FILLER                        PIC X(40).
