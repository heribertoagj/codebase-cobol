***************************** Top of Data ******************************
      *---------------------------------------------------------------*
      *        INC I#CHEQ04                                           *
      *        CADASTRO DE CHEQUE REDUZIDO / EQUIVALENCIA             *
      *        LRECL=025                                              *
      *---------------------------------------------------------------*
       01  CHEQ-REG.
           05  CHEQ-XAVE.
               10    CHEQ-AGE                   PIC 9(05)  COMP-3.
               10    CHEQ-RAZ                   PIC 9(05)  COMP-3.
               10    CHEQ-CTA                   PIC 9(07)  COMP-3.
           05  CHEQ-DIG                         PIC X.
           05  CHEQ-CPF-NUM                     PIC 9(09)  COMP-3.
           05  CHEQ-CPF-FIL                     PIC 9(04).
           05  CHEQ-CPF-CON                     PIC 9(02).
           05  CHEQ-QTD-FOL                     PIC X(01).
           05  CHEQ-TP-EX                       PIC X(01).
           05  CHEQ-IND                         PIC X(01).

