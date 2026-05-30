      *----------------------------------------------------------------*
      *  I#BVVEMG - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG012 - RETORNO                                     *
      *  REQUISICAO DE ALTERACAO DE MANUTENCAO DE DOMICILIO BANCARIO   *
      *----------------------------------------------------------------*

       01  BVVEMG-ASCG012.
           03 BVVEMG-ID-REG                    PIC X(001).
           03 BVVEMG-HEADER.
              05  BVVEMG-NOME-ARQ              PIC X(050).
              05  BVVEMG-NUM-CTRL-EMIS         PIC X(020).
              05  BVVEMG-NUM-CTRL-DEST-OR      PIC X(020).
              05  BVVEMG-ISPB-EMISSOR          PIC X(008).
              05  BVVEMG-ISPB-DESTINATARIO     PIC X(008).
              05  BVVEMG-DT-HR-ARQ             PIC X(019).
              05  BVVEMG-SIT-CONS              PIC X(002).
              05  BVVEMG-GRUPO-SEQ.
                  07  BVVEMG-NUM-SEQ           PIC 9(009).
                  07  BVVEMG-INDR-CONT         PIC X(001).
              05  BVVEMG-DT-REF                PIC X(008).
              05 FILLER                        PIC X(036).
           03 BVVEMG-DETALHE                  REDEFINES
              BVVEMG-HEADER.
              05 BVVEMG-SITUACAO              PIC X(001).
              05 BVVEMG-GRUPO-ACEITOS.
                 07 BVVEMG-AC-NUM-CTRL-IF     PIC X(020).
                 07 BVVEMG-AC-NUM-IDEN-SCG    PIC 9(017).
                 07 FILLER                    PIC X(143).
              05 BVVEMG-GRUPO-REJEITADOS      REDEFINES
                 BVVEMG-GRUPO-ACEITOS.
                 07 BVVEMG-REJ-NUM-CTRL-IF    PIC X(020).
                 07 BVVEMG-REJ-NUM-CTRL-IF-E  PIC X(008).
                 07 BVVEMG-REJ-NUM-COD-IF     PIC X(003).
                 07 BVVEMG-REJ-NUM-COD-IF-E   PIC X(008).
                 07 BVVEMG-REJ-NUM-IDEN-SCG   PIC 9(017).
                 07 BVVEMG-REJ-NUM-IDEN-SCG-E PIC X(008).
                 07 BVVEMG-REJ-AG-CLI         PIC 9(004).
                 07 BVVEMG-REJ-AG-CLI-E       PIC X(008).
                 07 BVVEMG-REJ-CT-CLI         PIC 9(013).
                 07 BVVEMG-REJ-CT-CLI-E       PIC X(008).
                 07 BVVEMG-REJ-PERC-SINAL     PIC X(001).
                 07 BVVEMG-REJ-PERC-GAR       PIC 9(005).
                 07 BVVEMG-REJ-PERC-GAR-E     PIC X(008).
                 07 BVVEMG-REJ-DTINI-VALIDD   PIC 9(008).
                 07 BVVEMG-REJ-DTINI-VALIDD-E PIC X(008).
                 07 BVVEMG-REJ-DTFIM-VALIDD   PIC 9(008).
                 07 BVVEMG-REJ-DTFIM-VALIDD-E PIC X(008).
                 07 BVVEMG-REJ-TP-NATU-GAR    PIC X(001).
                 07 BVVEMG-REJ-TP-NATU-GAR-E  PIC X(008).
                 07 BVVEMG-REJ-VLR-MAX-RET    PIC X(020).
                 07 BVVEMG-REJ-VLR-MAX-RET-E  PIC X(008).
