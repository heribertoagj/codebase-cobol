      *----------------------------------------------------------------*
      *  I#BVVELD - RETORNO OPTOUT ASCG102RET                          *
      *  ARQUIVO ASCG102R   - RETORNO  - TAMANHO 250
      *  RESPOSTA CANCEL.AUTORIZACAO DO CLIENTE PARA O ENVIO DE AGENDA *
      *----------------------------------------------------------------*

       01  BVVELD-ASCG102R.
           03 BVVELD-ID-REG                         PIC X(001).
           03 BVVELD-HEADER.
              05 BVVELD-NOME-ARQ                    PIC X(050).
              05 BVVELD-NUM-CTRL-EMIS               PIC X(020).
              05 BVVELD-NUM-CTRL-DEST-OR            PIC X(020).
              05 BVVELD-ISPB-EMISSOR                PIC X(008).
              05 BVVELD-ISPB-DESTINATARIO           PIC X(008).
              05 BVVELD-DT-HR-ARQ                   PIC X(019).
              05 BVVELD-SIT-CONS                    PIC X(002).
              05 BVVELD-GRUPO-SEQ.
                 07 BVVELD-NUM-SEQ                  PIC 9(009).
                 07 BVVELD-INDR-CONT                PIC X(001).
              05 BVVELD-DT-REF                      PIC X(008).
              05 FILLER                             PIC X(104).
           03 BVVELD-DETALHE             REDEFINES
              BVVELD-HEADER.
              05 BVVELD-SITUACAO                    PIC X(001).
              05 BVVELD-GRUPO-ACEITOS.
                 07 BVVELD-AC-NUM-CTRL-IF           PIC X(020).
                 07 BVVELD-AC-NUM-IDEN-SCG-CANC     PIC 9(020).
                 07 BVVELD-AC-NUM-IDEN-SCG          PIC 9(020).
                 07 FILLER                          PIC X(188).
              05 BVVELD-GRUPO-REJEITADOS REDEFINES
                 BVVELD-GRUPO-ACEITOS.
                 07 BVVELD-REJ-NUM-CTRL-IF          PIC X(020).
                 07 BVVELD-REJ-NUM-CTRL-IF-E        PIC X(008).
                 07 BVVELD-GRUPO-CLIENTE.
                    09 BVVELD-REJ-AC-NUM-IDEN-SCG   PIC X(020).
                    09 BVVELD-REJ-AC-NUM-IDEN-SCG-E PIC X(008).
                 07 FILLER                          PIC X(192).

      *  SITUACAO 'A' ACEITO  OU 'R' REJEITADO
      *  DT-HR-ARQ NO FORMATO 9999-99-99-99:99:99

