      *----------------------------------------------------------------*
      *    INC I#BVVE00                                                *
      *    INPUT  :  INSTITUICOES/ASSOCIACOES VISA E MASTERCARD        *
      *              ORG. SEQUENCIAL   -   LRECL = 0006                *
      *----------------------------------------------------------------*

       01  REG-CADINST.
           03  BVVE00-INSTASSO            PIC 9(07) COMP-3.
           03  BVVE00-TPESSOA             PIC X(01).
           03  BVVE00-BANDEIRA            PIC X(01).
