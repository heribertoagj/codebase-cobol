      *----------------------------------------------------------------*
      *  BOOK DE ARQUIVOS RECEBIDOS DA CIP                             *
      *----------------------------------------------------------------*

       01  REG-BVVEDI.
           03  BVVEDI-HEADER.
               05  BVVEDI-NOME-ARQ          PIC X(050).
               05  BVVEDI-NUM-CTRL-EMIS     PIC X(020).
               05  BVVEDI-NUM-CTRL-DEST-OR  PIC X(020).
               05  BVVEDI-ISPB-EMISSOR      PIC X(008).
               05  BVVEDI-ISPB-DESTINATARIO PIC X(008).
               05  BVVEDI-DT-HR-ARQ         PIC X(019).
               05  BVVEDI-SIT-CONS          PIC X(002).
               05  BVVEDI-GRUPO-SEQ.
                   07  BVVEDI-NUM-SEQ       PIC 9(009).
                   07  BVVEDI-INDR-CONT     PIC X(001).
               05  BVVEDI-DT-REF            PIC X(008).
               05  FILLER                   PIC X(31855).

           03  BVVEDI-DETALHE REDEFINES BVVEDI-HEADER.
               05 BVVEDI-SEQ-REGISTRO      PIC 9(001).
               05 BVVEDI-ID-CONTINUIDADE   PIC X(001).
               05 BVVEDI-FILLER            PIC X(006).
               05 BVVEDI-REGISTRO.
                  07  FILLER               PIC X(31992).
