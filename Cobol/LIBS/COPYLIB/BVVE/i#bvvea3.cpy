      *  BOOK DE ARQUIVOS RECEBIDOS DA CIP                             *
      *----------------------------------------------------------------*

       01  REG-BVVEA3.
           03  BVVEA3-HEADER.
               05  BVVEA3-NOME-ARQ          PIC X(050).
               05  BVVEA3-NUM-CTRL-EMIS     PIC X(020).
               05  BVVEA3-NUM-CTRL-DEST-OR  PIC X(020).
               05  BVVEA3-ISPB-EMISSOR      PIC X(008).
               05  BVVEA3-ISPB-DESTINATARIO PIC X(008).
               05  BVVEA3-DT-HR-ARQ         PIC X(019).
               05  BVVEA3-SIT-CONS          PIC X(002).
               05  BVVEA3-GRUPO-SEQ.
                   07  BVVEA3-NUM-SEQ       PIC 9(009).
                   07  BVVEA3-INDR-CONT     PIC X(001).
               05  BVVEA3-DT-REF            PIC X(008).
               05  FILLER                   PIC X(31855).

           03  BVVEA3-DETALHE REDEFINES BVVEA3-HEADER.
               05 BVVEA3-REGISTRO.
                  07  FILLER               PIC X(32000).
