      *================================================================*
      *  ARQUIVO VSAM DE PARAMETRIZACAO DE BANDEIRAS X PRODUTOS BVVE   *
      *  DSN  : CB.BACN.VON.BANDPROD                                   *
      *  LRECL: 140   CHAVE: 030                                       *
      *================================================================*
       01  BVVEAK-BANDPROD.
           05  BVVEAK-CHAVE.
               10  BVVEAK-CH-BANDEIRA      PIC  X(004).
               10  BVVEAK-CH-PRODUTO       PIC  9(002).
               10  BVVEAK-CH-FILLER        PIC  X(024).
           05  BVVEAK-CREDENCIADORA        PIC  X(010).
           05  BVVEAK-TP-PROD              PIC  X(001).
      *        C = CREDITO
      *        D = DEBITO
           05  BVVEAK-MODALIDADE           PIC  X(001).
      *        P = PARCELADO
      *        R = ROTATIVO
           05  BVVEAK-HATULZ               PIC  X(026).
           05  BVVEAK-FILLER               PIC  X(072).
