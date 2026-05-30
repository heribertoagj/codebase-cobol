      *----------------------------------------------------------------*
      *  I#BVVEMH - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG013 - RETORNO COM SUCESSO                         *
      *  REQUISICAO DE EXCLUSAO DE MANUTENCAO DE DOMICILIO BANCARIO    *
      *----------------------------------------------------------------*

       01  BVVEMH-ASCG013.
           03 BVVEMH-ID-REG                    PIC X(001).
           03 BVVEMH-HEADER.
              05  BVVEMH-NOME-ARQ              PIC X(050).
              05  BVVEMH-NUM-CTRL-EMIS         PIC X(020).
              05  BVVEMH-NUM-CTRL-DEST-OR      PIC X(020).
              05  BVVEMH-ISPB-EMISSOR          PIC X(008).
              05  BVVEMH-ISPB-DESTINATARIO     PIC X(008).
              05  BVVEMH-DT-HR-ARQ             PIC X(019).
              05  BVVEMH-SIT-CONS              PIC X(002).
              05  BVVEMH-GRUPO-SEQ.
                  07  BVVEMH-NUM-SEQ           PIC 9(009).
                  07  BVVEMH-INDR-CONT         PIC X(001).
              05  BVVEMH-DT-REF                PIC X(008).
           03 BVVEMH-DETALHE                   REDEFINES
              BVVEMH-HEADER.
              05 BVVEMH-SITUACAO               PIC X(001).
              05 BVVEMH-GRUPO-ACEITOS.
                 07 BVVEMH-AC-NUM-CTRL-IF      PIC X(020).
                 07 BVVEMH-AC-NUM-IDEN-SCG     PIC 9(017).
                 07 FILLER                     PIC X(107).
              05 BVVEMH-GRUPO-REJEITADOS       REDEFINES
                 BVVEMH-GRUPO-ACEITOS.
                 07 BVVEMH-REJ-NUM-CTRL-IF     PIC X(020).
                 07 BVVEMH-REJ-NUM-CTRL-IF-E   PIC X(008).
                 07 BVVEMH-REJ-NUM-IDEN-SCG    PIC 9(017).
                 07 BVVEMH-REJ-NUM-IDEN-SCG-E  PIC X(008).
                 07 BVVEMH-REJ-TP-BAIXA        PIC X(001).
                 07 BVVEMH-REJ-TP-BAIXA-E      PIC X(008).
                 07 FILLER                     PIC X(082).

