      ******************************************************************
      *    I#BVVERE - LAY-OUT DO ARQUIVO DE RESTRICOES                 *
      *    LRECL: 100                                                  *
      ******************************************************************
       01  PARM-RESTRICAO.
           03  PARM-REST-CH.
               05  PARM-REST-CH-CODIGO       PIC 9(02).
           03  PARM-REST-DESCRICAO           PIC X(30).
           03  PARM-REST-DT-INCLUSAO         PIC 9(08).
           03  PARM-REST-USUARIO             PIC X(09).
           03  FILLER                        PIC X(51).
