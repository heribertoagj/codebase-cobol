      ******************************************************************
      *    I#BVVEEM - LAY-OUT DO ARQUIVO DE TIPOS DE EMPRESAS          *
      *    LRECL: 150                                                  *
      ******************************************************************
       01  PARM-EMPRESA.
           03  PARM-EMPR-CH.
               05  PARM-EMPR-CH-CODIGO       PIC 9(02).
           03  PARM-EMPR-DESCRICAO           PIC X(70).
           03  PARM-EMPR-DT-INCLUSAO         PIC 9(08).
           03  PARM-EMPR-USUARIO             PIC X(09).
           03  FILLER                        PIC X(61).
