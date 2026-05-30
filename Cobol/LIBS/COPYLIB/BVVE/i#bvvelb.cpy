      *----------------------------------------------------------------*
      *  I#BVVELB - RETORNO OPT-IN ASCG101RET                          *
      *  ARQUIVO ASCG101R   - RETORNO  - TAMANHO 250                   *
      *  RESPOSTA DA INCLUSAO DE AUTORIZ. CLIENTE DE ENVIO DE AGENDA   *
      *----------------------------------------------------------------*

       01  BVVELB-ASCG101R.
           03 BVVELB-ID-REG                       PIC X(001).
           03 BVVELB-HEADER.
              05 BVVELB-NOME-ARQ                  PIC X(050).
              05 BVVELB-NUM-CTRL-EMIS             PIC X(020).
              05 BVVELB-NUM-CTRL-DEST-OR          PIC X(020).
              05 BVVELB-ISPB-EMISSOR              PIC X(008).
              05 BVVELB-ISPB-DESTINATARIO         PIC X(008).
              05 BVVELB-DT-HR-ARQ                 PIC X(019).
              05 BVVELB-SIT-CONS                  PIC X(002).
              05 BVVELB-GRUPO-SEQ.
                 07 BVVELB-NUM-SEQ                PIC 9(009).
                 07 BVVELB-INDR-CONT              PIC X(001).
              05 BVVELB-DT-REF                    PIC X(008).
              05 FILLER                           PIC X(104).
           03 BVVELB-DETALHE                      REDEFINES
              BVVELB-HEADER.
              05 BVVELB-SITUACAO                  PIC X(001).
              05 BVVELB-GRUPO-ACEITOS.
                 07 BVVELB-AC-NUM-CTRL-IF         PIC X(020).
                 07 BVVELB-AC-NUM-IDEN-SCG        PIC 9(020).
                 07 FILLER                        PIC X(208).
              05 BVVELB-GRUPO-REJEITADOS REDEFINES
                 BVVELB-GRUPO-ACEITOS.
                 07 BVVELB-REJ-NUM-CTRL-IF        PIC X(020).
                 07 BVVELB-REJ-NUM-CTRL-IF-E      PIC X(008).
                 07 BVVELB-GRUPO-CLIENTE.
                    09 BVVELB-REJ-TP-PESSOA-CLI   PIC X(001).
                    09 BVVELB-REJ-TP-PESSOA-CLI-E PIC X(008).
                    09 BVVELB-REJ-CNPJ-CPF-CLI    PIC X(014).
                    09 BVVELB-REJ-CNPJ-CPF-CLI-E  PIC X(008).
                 07 BVVELB-REJ-CNPJ-BASE-CLI      PIC X(008).
                 07 BVVELB-REJ-CNPJ-BASE-CLI-E    PIC X(008).
                 07 BVVELB-REJ-COD-ARRAJ-PAGTO    PIC X(003).
                 07 BVVELB-REJ-COD-ARRAJ-PAGTO-E  PIC X(008).
                 07 BVVELB-REJ-IND-DOMCL          PIC X(001).
                 07 BVVELB-REJ-IND-DOMCL-E        PIC X(008).
                 07 BVVELB-REJ-CNPJ-CREDDR        PIC X(014)           .
                 07 BVVELB-REJ-CNPJ-CREDDR-E      PIC X(008).
                 07 FILLER                        PIC X(131).
      *  SITUACAO 'A' ACEITO  OU 'R' REJEITADO
      *  DT-HR-ARQ NO FORMATO 9999-99-99-99:99:99
      *  CPF ALINHADO A DIREITA COM ZEROS A ESQUERDA
