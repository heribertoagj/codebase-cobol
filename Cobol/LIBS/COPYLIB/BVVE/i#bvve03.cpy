      *---------------------------------------------------------------*
      * -INC I#BVVE03                                                 *
      *                                                               *
      *      ARQUIVO A SER ENVIADO PARA A ROTINA DESO - LRECL = 040   *
      *                                                               *
      *          SISTEMA PARCELADO LOJISTA - ARQUIVO PARA O DESO      *
      *                                                               *
      *           UTILIZACAO: CARTEIRA "D" - ANTECIPACAO DE CREDITOS  *
      *                                    ** PARCELADOS **           *
      *                                                               *
      *                         KAMBARA - GP87 - DL06 - DEZEMBRO/1998 *
      *---------------------------------------------------------------*

      *-========================--===================================-*
       01  DESO-REGISTRO.
           03  DESO-COD-AGE                    PIC 9(005)      COMP-3.
           03  DESO-NRO-CTA                    PIC 9(007)      COMP-3.
           03  DESO-COD-ORPAG                  PIC 9(002).
           03  DESO-SALDO-ANTECIPADO           PIC S9(11)V99   COMP-3.
           03  DESO-DATA-AAAAMMDD              PIC 9(09)       COMP-3.
           03  FILLER                          PIC X(019).

      *-========================--===================================-*
