      *================================================================*
      *  I#BVVEDX - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  PARAMETRIZACAO DOS BANCOS CONCORRENTES DE DOMICILIO           *
      *----------------------------------------------------------------*
      *  DSNAME       : CB.BACC.VON.BANCPDOM
      *  LRECL ARQUIVO: 090                                            *
      *  LRECL CHAVE  : 020                                            *
      *================================================================*

       01  BVVEDX-BANCPDOM.
           05 BVVEDX-CHAVE.
               10 BVVEDX-CH-CODIGO             PIC  9(003).
               10 BVVEDX-CH-FILLER             PIC  X(017).
           05 BVVEDX-DESCRICAO                 PIC  X(020).
           05 BVVEDX-TIMESTAMP                 PIC  X(020).
           05 FILLER                           PIC  X(030).
