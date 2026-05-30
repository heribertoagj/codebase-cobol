      *----------------------------------------------------------------*
      *  I#BVVEMF - MANUTENCAO DE DOMICILIO BANCARIO                   *
      *  ARQUIVO ASCG011R - RETORNO                                    *
      *  REQUISICAO DE INCLUSAO DE MANUTENCAO DE DOMICILIO BANCARIO    *
      *----------------------------------------------------------------*

       01  BVVEMF-ASCG011.
           03 BVVEMF-ID-REG                       PIC X(001).
           03 BVVEMF-HEADER.
              05 BVVEMF-NOME-ARQ                  PIC X(050).
              05 BVVEMF-NUM-CTRL-EMIS             PIC X(020).
              05 BVVEMF-NUM-CTRL-DEST-OR          PIC X(020).
              05 BVVEMF-ISPB-EMISSOR              PIC X(008).
              05 BVVEMF-ISPB-DESTINATARIO         PIC X(008).
              05 BVVEMF-DT-HR-ARQ                 PIC X(019).
              05 BVVEMF-SIT-CONS                  PIC X(002).
              05 BVVEMF-GRUPO-SEQ.
                 07 BVVEMF-NUM-SEQ                PIC 9(009).
                 07 BVVEMF-INDR-CONT              PIC X(001).
              05 BVVEMF-DT-REF                    PIC X(008).
              05 FILLER                           PIC X(104).
           03 BVVEMF-DETALHE                      REDEFINES
              BVVEMF-HEADER.
              05 BVVEMF-SITUACAO                  PIC X(001).
              05 BVVEMF-GRUPO-ACEITOS.
                 07 BVVEMF-AC-NUM-CTRL-IF         PIC X(020).
                 07 BVVEMF-AC-NUM-IDEN-SCG        PIC 9(017).
                 07 FILLER                        PIC X(197).
              05 BVVEMF-GRUPO-REJEITADOS REDEFINES
                 BVVEMF-GRUPO-ACEITOS.
                 07 BVVEMF-REJ-NUM-CTRL-IF        PIC X(020).
                 07 BVVEMF-REJ-NUM-CTRL-IF-E      PIC X(008).
                 07 BVVEMF-GRUPO-CLIENTE.
                    09 BVVEMF-REJ-TP-PESSOA-CLI   PIC X(001).
                    09 BVVEMF-REJ-TP-PESSOA-CLI-E PIC X(008).
                    09 BVVEMF-REJ-CNPJ-CPF-CLI    PIC X(014).
                    09 BVVEMF-REJ-CNPJ-CPF-CLI-E  PIC X(008).
                 07 BVVEMF-REJ-CNPJ-BASE-CLI      PIC X(008).
                 07 BVVEMF-REJ-CNPJ-BASE-CLI-E    PIC X(008).
                 07 BVVEMF-REJ-COD-PROPT-ESQM     PIC X(003).
                 07 BVVEMF-REJ-COD-PROPT-ESQM-E   PIC X(008).
                 07 BVVEMF-REJ-TP-PRODT           PIC X(001).
                 07 BVVEMF-REJ-TP-PROPT-E         PIC X(008).
                 07 BVVEMF-REJ-NUM-COD-IF         PIC X(003).
                 07 BVVEMF-REJ-NUM-COD-IF-E       PIC X(008).
                 07 BVVEMF-REJ-AG-CLI             PIC 9(004).
                 07 BVVEMF-REJ-AG-CLI-E           PIC X(008).
                 07 BVVEMF-REJ-CT-CLI             PIC 9(013).
                 07 BVVEMF-REJ-CT-CLI-E           PIC X(008).
                 07 BVVEMF-REJ-SINAL-PER          PIC X(001).
                 07 BVVEMF-REJ-PERC-GAR           PIC 9(005).
                 07 BVVEMF-REJ-PERC-GAR-E         PIC X(008).
                 07 BVVEMF-REJ-DTINI-VALIDD       PIC 9(008).
                 07 BVVEMF-REJ-DTINI-VALIDD-E     PIC X(008).
                 07 BVVEMF-REJ-DTFIM-VALIDD       PIC 9(008).
                 07 BVVEMF-REJ-DTFIM-VALIDD-E     PIC X(008).
                 07 BVVEMF-REJ-TP-NATU-GAR        PIC X(001).
                 07 BVVEMF-REJ-TP-NATU-GAR-E      PIC X(008).
                 07 BVVEMF-REJ-VLR-MAX-RETEN      PIC X(020).
                 07 BVVEMF-REJ-VLR-MAX-RETEN-E    PIC X(008).
      *  SITUACAO 'A' ACEITO  OU 'R' REJEITADO
      *  DT-HR-ARQ NO FORMATO 9999-99-99-99:99:99
      *  CPF ALINHADO A DIREITA COM ZEROS A ESQUERDA
