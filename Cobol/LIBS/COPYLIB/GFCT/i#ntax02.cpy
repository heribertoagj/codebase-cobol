      *---------------------------------------------------------------*
      *                   INC I#NTAX02                                *
      *     DSN = AC.NTAX.PRV.ARQNTAX(0)    ALPHAVILLE  PRODUCAO      *
      *     DSN = MX.NTAX.PRP.ARQNTAX(0)    MATRIZ      PRODUCAO      *
      *     DSN = AV.NTAX.ARQNTAX(0)        ALPHAVILLE  TESTE         *
      *                ORG. SEQUENCIAL  - LRECL = 060                 *
      *---------------------------------------------------------------*

       01   REG-TAX2.
            05 TAX2-TIPO             PIC 9(05)      COMP-3.
            05 TAX2-PERIODO.
               07 TAX2-DT-INIC       PIC 9(09)      COMP-3.
               07 TAX2-DT-FIM        PIC 9(09)      COMP-3.
            05 TAX2-VR-ATU           PIC 9(13)      COMP-3.
            05 TAX2-VR-ATU-R REDEFINES
                    TAX2-VR-ATU      PIC S9(11)V99  COMP-3.
            05 TAX2-VR-ANTERIOR      PIC 9(13)      COMP-3.
            05 TAX2-IND-CORRECAO     PIC 9(13)      COMP-3.
            05 TAX2-IND-ACUMULADO    PIC 9(15)      COMP-3.
            05 TAX2-RESERVA          PIC X(13).
            05 TAX2-DATA-ALTER       PIC 9(09)      COMP-3.
      *************    FIM  DA   INC I#NTAX02     *********************
