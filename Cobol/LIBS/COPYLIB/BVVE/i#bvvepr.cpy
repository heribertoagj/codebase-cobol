      ******************************************************************
      *    I#BVVEPR - LAY-OUT DO ARQUIVO DE PRODUTOS CONTRATADOS       *
      *    LRECL: 100                                                  *
      ******************************************************************
       01  PARM-PRODUTO.
           03  PARM-PROD-CH.
               05  PARM-PROD-CH-CODIGO       PIC 9(03).
           03  PARM-PROD-DESCRICAO           PIC X(30).
           03  PARM-PROD-DT-INCLUSAO         PIC 9(08).
           03  PARM-PROD-USUARIO             PIC X(09).
           03  FILLER                        PIC X(50).
