      *----------------------------------------------------------------*
      *  I#BVVEA8 - IQUIDACAO FINANCEIRA CIP                           *
      *  ARQUIVO ASCG023 E ASC025 EXTRAIDOS                            *
      *----------------------------------------------------------------*

       01  BVVEA8-ASCG023.
           03 BVVEA8-ID-REG                       PIC X(001).
           03 BVVEA8-HEADER.
              05 BVVEA8-NOME-ARQ                  PIC X(050).
              05 BVVEA8-NUM-CTRL-EMIS             PIC X(020).
              05 BVVEA8-NUM-CTRL-DEST-OR          PIC X(020).
              05 BVVEA8-ISPB-EMISSOR              PIC X(008).
              05 BVVEA8-ISPB-DESTINATARIO         PIC X(008).
              05 BVVEA8-DT-HR-ARQ                 PIC X(019).
              05 BVVEA8-SIT-CONS                  PIC X(002).
              05 BVVEA8-GRUPO-SEQ.
                 07 BVVEA8-NUM-SEQ                PIC 9(009).
                 07 BVVEA8-INDR-CONT              PIC X(001).
              05 BVVEA8-DT-REF                    PIC X(008).
           03 BVVEA8-DETALHE                      REDEFINES
              BVVEA8-HEADER.
              05 BVVEA8-SITUACAO                  PIC X(001).
              05 BVVEA8-GRUPO-ACEITOS.
                 07 BVVEA8-AC-NUM-LIQUID          PIC X(021).
                 07 BVVEA8-AC-NUM-LIQUID-E        PIC X(008).
                 07 BVVEA8-AC-NUM-CTRL-IF         PIC X(020).
                 07 BVVEA8-AC-NUM-CTRL-CIP        PIC X(020).
                 07 BVVEA8-AC-DTHR-MANUT          PIC X(019).
                 07 BVVEA8-FILLER                 PIC X(056).
              05 BVVEA8-GRUPO-REJEITADOS REDEFINES
                 BVVEA8-GRUPO-ACEITOS.
                 07 BVVEA8-REJ-NUM-LIQUID         PIC X(021).
                 07 BVVEA8-REJ-NUM-LIQUID-E       PIC X(008).
                 07 BVVEA8-REJ-NUM-CTRL-IF        PIC X(020).
                 07 BVVEA8-REJ-NUM-CTRL-IF-E      PIC X(008).
                 07 BVVEA8-REJ-OCORR  OCCURS 5 TIMES.
                    09 BVVEA8-REJ-COD-OCORR       PIC X(002).
                    09 BVVEA8-REJ-COD-OCORR-E     PIC X(008).
                 07 BVVEA8-FILLER                 PIC X(037).
      *  SITUACAO 'A' ACEITO  OU 'R' REJEITADO
      *  DT-HR-ARQ NO FORMATO 9999-99-99-99:99:99
