      ***===========================================================***
      *---------------------------------------------------------------*
      *  -INC I#BVVEKK                                                *
      *       ARQUIVO AGENDA ESPECIAL DE 20.04.2000                   *
      *       ***  SOMENTE EM MOEDA REAL  (R$)   ***                  *
      *              LRECL = 085                                      *
      *                              KAMBARA - GP87 - 30.03.2000      *
      *                                                               *
      *---------------------------------------------------------------*
      *
       01  REG-VISANET.
           05  REVS-BANCO                PIC 9(03).
           05  REVS-ESTABELEC            PIC 9(10).
           05  REVS-AGENCIA              PIC 9(05).
           05  FILLER                    PIC X.
           05  REVS-CONTA                PIC X(14).
           05  REVS-VRBRU                PIC 9(13)V99.
           05  FILLER                    PIC X.
           05  REVS-VRLIQ                PIC 9(13)V99.
           05  FILLER                    PIC X.
           05  REVS-VRCOM                PIC 9(13)V99.
           05  FILLER                    PIC X.
           05  REVS-FLAG                 PIC X(01).
           05  FILLER                    PIC X(03).
      ***===========================================================***
