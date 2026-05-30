      *----------------------------------------------------------------*
      *  I#BVVED1 - ARQUIVO DE PARAMETRIZACAO DE BANDEIRAS             *
      *  DSNAME: CB.BACC.VON.TRAVPBAN
      *----------------------------------------------------------------*

       01  BVVED1-TRAVPBAN.
           03 BVVED1-CHAVE.
              05 BVVED1-COD-BAND       PIC 9(003).
              05 BVVED1-FILLER-CHV     PIC X(017).
           03 BVVED1-DESC-BAND         PIC X(040).
           03 BVVED1-IND-TRAVA         PIC 9(003).
           03 BVVED1-COD-PROP-ESQ-X.
              05 BVVED1-COD-PROP-ESQ   PIC 9(003).
           03 FILLER                   PIC X(054).
