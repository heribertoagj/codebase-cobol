      *----------------------------------------------------------------*
      *  I#BVVEB5 - ARQUIVO TEMPORARIO PARA ENVIO AO CACF              *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *  FORMATO DATA/HORA: AAAA-MM-DDTHH:MM:SS:MMMMMM                 *
      *----------------------------------------------------------------*

       01  REG-BVVEB5.
           03  BVVEB5-NUM-LIQUIDACAO       PIC X(21).
           03  BVVEB5-BANDEIRA             PIC X(03).
           03  BVVEB5-IDENTD-PART-PRINC    PIC X(08).
           03  BVVEB5-IDENTD-PART-ADM      PIC X(08).
           03  BVVEB5-ISPB-DEVEDOR         PIC X(08).
           03  BVVEB5-ISPB-CREDORA         PIC X(08).
           03  BVVEB5-DT-PGTO              PIC 9(08).
           03  BVVEB5-TP-PRODT             PIC 9(02).
           03  BVVEB5-IDTF-TRANSF          PIC 9(01).
           03  FILLER                      PIC X(33).

