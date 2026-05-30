      *----------------------------------------------------------------*
      *    INC I#BVVE10                                                *
      *    INPUT  :  QTDE CARTOES VISA E MASTERCARD                    *
      *              ORG. SEQUENCIAL   -   LRECL = 0050                *
      *----------------------------------------------------------------*

       01  REG-QTDECART.
           03  BVVE10-AGENCIA             PIC 9(05) COMP-3.
           03  BVVE10-CONTA               PIC 9(07) COMP-3.
           03  BVVE10-QTDE-VISA           PIC 9(15) COMP-3.
           03  BVVE10-QTDE-MASTERCARD     PIC 9(15) COMP-3.
           03  BVVE10-INSTASSO            PIC 9(07) COMP-3.
           03  BVVE10-CARTAO              PIC 9(16) COMP-3.
           03  FILLER                     PIC X(14).
