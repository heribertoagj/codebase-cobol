      *----------------------------------------------------------------*
      *  I#BVVEB2 - IQUIDACAO FINANCEIRA CIP                           *
      *  ARQUIVO ASLC023R/ASLC025R/ASLC033R EXTRAIDOS                  *
      *----------------------------------------------------------------*

       01  BVVEB2-ASLC-RET.
           03 BVVEB2-ID-REG                       PIC X(001).
           03 BVVEB2-HEADER.
              05 BVVEB2-NOME-ARQ                  PIC X(050).
              05 BVVEB2-NUM-CTRL-EMIS             PIC X(020).
              05 BVVEB2-NUM-CTRL-DEST-OR          PIC X(020).
              05 BVVEB2-ISPB-EMISSOR              PIC X(008).
              05 BVVEB2-ISPB-DESTINATARIO         PIC X(008).
              05 BVVEB2-DT-HR-ARQ                 PIC X(019).
              05 BVVEB2-SIT-CONS                  PIC X(002).
              05 BVVEB2-GRUPO-SEQ.
                 07 BVVEB2-NUM-SEQ                PIC 9(009).
                 07 BVVEB2-INDR-CONT              PIC X(001).
              05 BVVEB2-DT-REF                    PIC X(008).
           03 BVVEB2-DETALHE                      REDEFINES
              BVVEB2-HEADER.
              05 BVVEB2-SITUACAO                  PIC X(001).
              05 BVVEB2-GRUPO-ACEITOS.
                 07 BVVEB2-AC-NUM-LIQUID          PIC X(021).
                 07 BVVEB2-AC-ID-PART-PRIN        PIC X(008).
                 07 BVVEB2-AC-ID-PART-ADM         PIC X(008).
                 07 BVVEB2-AC-NUM-CTRL-IF         PIC X(020).
                 07 BVVEB2-AC-NUM-CTRL-CIP        PIC X(020).
                 07 BVVEB2-AC-DTHR-MANUT          PIC X(019).
                 07 FILLER                        PIC X(043).
              05 BVVEB2-GRUPO-REJEITADOS REDEFINES
                 BVVEB2-GRUPO-ACEITOS.
                 07 BVVEB2-REJ-NUM-LIQUID         PIC X(021).
                 07 BVVEB2-REJ-NUM-LIQUID-E       PIC X(008).
                 07 BVVEB2-REJ-ID-PART-PRIN       PIC X(008).
                 07 BVVEB2-REJ-ID-PART-PRIN-E     PIC X(008).
                 07 BVVEB2-REJ-ID-PART-ADM        PIC X(008).
                 07 BVVEB2-REJ-ID-PART-ADM-E      PIC X(008).
                 07 BVVEB2-REJ-NUM-CTRL-IF        PIC X(020).
                 07 BVVEB2-REJ-NUM-CTRL-IF-E      PIC X(008).
                 07 BVVEB2-REJ-OCORR  OCCURS 5 TIMES.
                    09 BVVEB2-REJ-COD-OCORR       PIC X(002).
                    09 BVVEB2-REJ-COD-OCORR-E     PIC X(008).
      *  SITUACAO 'A' ACEITO  OU 'R' REJEITADO
      *  DT-HR-ARQ NO FORMATO 9999-99-99-99:99:99
