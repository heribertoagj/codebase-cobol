      *---------------------------------------------------------------*
      * I#DCOMCT - ARQUIVO DE CONTROLE   -    LRECL = 0150            *
      *---------------------------------------------------------------*

       01  REG-CONTROLE.
           05  HEADER-CONTR            PIC X(44).
           05  FILLER REDEFINES HEADER-CONTR.
               10  PARM-CONTR.
                   15  LOC-IMPR-CONTR  PIC X(04).
                   15  CCUSTO-CONTR    PIC X(04).
                   15  JOBNAME-CONTR   PIC X(08).
               10  DATA-MVTO-CONTR     PIC 9(08).
               10  FILLER REDEFINES DATA-MVTO-CONTR.
                   15  DIA-MVTO-CONTR  PIC 9(02).
                   15  MES-MVTO-CONTR  PIC 9(02).
                   15  SEC-MVTO-CONTR  PIC 9(02).
                   15  ANO-MVTO-CONTR  PIC 9(02).
               10  DATA-SIST-CONTR     PIC 9(08).
               10  HORA-SIST-CONTR     PIC 9(06).
               10  VOLUME-CONTR        PIC X(06).
           05  QTDE-IMPRESS-CONTR      PIC 9(15).
           05  TOT-GERAL-CONTR         PIC 9(15).
           05  TOT-IMPRESSOS-CONTR     PIC 9(15).
           05  DOC-INIC-RST-CONTR      PIC 9(15).
           05  DOC-FIN-RST-CONTR       PIC 9(15).
           05  STATUS-CONTR            PIC X(01).
           05  TIPO-PROCESS-CONTR      PIC X(01).
           05  TIPO-FORM-CONTR         PIC 9(02).
           05  TOT-DOCTO-CONTR         PIC 9(15).
           05  FILLER                  PIC X(12).
