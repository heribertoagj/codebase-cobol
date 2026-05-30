           SKIP2
      ** AJAG - I#AJAG04 **********************************************
      *        INC        - I#AJAG04                                  *
      *        LRECL      - 41                                        *
      *        COMENTARIO - ARQUIVO DE MOVIMENTO PARA ROTINA AJAG     *
      *                     CONVERSAO ANO 2000 - COPIA DA I#AJUP04.   *
      *                                                               *
      *****************************************************************
           SKIP2
       01  REG-MOVTO-AJAG.
           05  AJAG-JUNCAO               PIC 9(05)    COMP-3.
           05  AJAG-DIGITO               PIC X.
           05  AJAG-RAZAO                PIC 9(05)    COMP-3.
           05  AJAG-COD-SERV             PIC 9(03)    COMP-3.
           05  AJAG-SEQ                  PIC 9(05)    COMP-3.
           05  AJAG-VALOR                PIC 9(15)V99 COMP-3.
           05  AJAG-DC                   PIC 9.
           05  AJAG-CENT-CUSTO           PIC X(04).
           05  AJAG-DT-MOVTO             PIC 9(09)    COMP-3.
           05  AJAG-RESERVA              PIC X(10).

      *****************************************************************
      *                 DADOS A RESPEITO DOS CAMPOS                   *
      *  CAMPO                     COMENTARIO                         *
      *  AJAG-DC                   VALORES = 6 DEBITO, 7 CREDITO      *
      *  AJAG-DT-MOVTO             AAAAMMDD                           *
      *****************************************************************
