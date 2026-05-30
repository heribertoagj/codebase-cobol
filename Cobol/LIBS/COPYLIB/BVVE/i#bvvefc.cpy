      *----------------------------------------------------------------*
      *  I#BVVEFC - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  AGENDA FINANCEIRA CIP EXTRAIDA
      *  TAMANHO: 500                                                  *
      *----------------------------------------------------------------*

       01  BVVEFC-REGISTRO.
           03 BVVEFC-CNPJ-CREDENC                 PIC 9(014).
           03 BVVEFC-ARR-PGTO                     PIC X(003).
           03 BVVEFC-IND-USU-TIT                  PIC X(001).
           03 BVVEFC-GRUPO-USU-FINAL.
              05 BVVEFC-U-CNPJ-CPF-USUFIN         PIC 9(014).
              05 BVVEFC-U-VLR-LIV-USU-FNAL        PIC X(020).
              05 BVVEFC-U-UNID-RECEB.
                 07 BVVEFC-U-DT-PREV-LIQ          PIC X(008).
                 07 BVVEFC-U-VLR-TOT              PIC X(020).
                 07 BVVEFC-U-IND-DOMCL            PIC X(001).
                 07 BVVEFC-U-TITULAR.
                    09 BVVEFC-U-CNPJ-CPF-TIT      PIC 9(014).
                    09 BVVEFC-U-TP-PSSOA          PIC X(001).
                    09 BVVEFC-U-VLR-TOT-TITU      PIC X(020).
                    09 BVVEFC-U-VLR-COMP-OUTRA    PIC X(020).
                    09 BVVEFC-U-VLR-COMP-INST     PIC X(020).
                    09 BVVEFC-U-VLR-LIVR-TOT      PIC X(020).
                    09 BVVEFC-U-VLR-LIVR-ANT-CRED PIC X(020).
                    09 BVVEFC-U-VLR-PRE-CONTR     PIC X(020).
                    09 BVVEFC-U-VLR-ONUS-RESER    PIC X(020).
                    09 BVVEFC-U-DOMCL-BANC.
                       11 BVVEFC-U-CNPJ-TIT-CTA   PIC 9(014).
                       11 BVVEFC-U-TP-CTA         PIC X(002).
                       11 BVVEFC-U-AGENCIA.
                          13 BVVEFC-U-AGENCIA-N   PIC 9(004).
                       11 BVVEFC-U-CONTA.
                          13 BVVEFC-U-CONTA-N     PIC 9(013).
                       11 BVVEFC-U-CTA-PGTO       PIC X(020).
                       11 BVVEFC-U-DT-EFET-PGTO   PIC X(008).
                       11 BVVEFC-U-VLR-EFET       PIC X(020).
                       11 BVVEFC-U-VLR-LIVRE      PIC X(020).
                       11 BVVEFC-U-IND-INST       PIC X(001).
                       11 BVVEFC-U-REC-INSTIT.
                          13 BVVEFC-U-ID-OPER       PIC X(019).
                          13 BVVEFC-U-PRIORIDADE    PIC 9(009).
                          13 BVVEFC-U-CNPJ-REGIS    PIC 9(014).
                          13 BVVEFC-U-VLR-NEGOC     PIC X(020).
                          13 BVVEFC-U-VLR-A-CONS    PIC X(020).
                          13 BVVEFC-U-INDR-REGR-DIV PIC X(001).
                          13 BVVEFC-U-DT-FIM-OPER   PIC X(008).
                    09 BVVEFC-U-OUTR-INST.
                       11 BVVEFC-U-INDR-REGR-DIV-O  PIC X(001).
                       11 BVVEFC-U-VLR-NEGOC-O      PIC X(020).
                       11 BVVEFC-U-VLR-A-CONS-O     PIC X(020).
                       11 BVVEFC-U-DT-FIM-OPER-O    PIC X(008).
                       11 BVVEFC-U-PRIORIDADE-O     PIC 9(009).
      *
           03 BVVEFC-TITULAR                      REDEFINES
              BVVEFC-GRUPO-USU-FINAL.
              05 BVVEFC-T-CNPJ-CPF-TITULAR        PIC 9(014).
              05 BVVEFC-T-UNID-RECEB.
                 07 BVVEFC-T-DT-PREV-LIQ          PIC X(008).
                 07 BVVEFC-T-VLR-TOT-TIT          PIC X(020).
                 07 BVVEFC-T-VLR-COMP-OUTRA       PIC X(020).
                 07 BVVEFC-T-VLR-COMP-INST        PIC X(020).
                 07 BVVEFC-T-VLR-LIVR-TOT         PIC X(020).
                 07 BVVEFC-T-VLR-LIVR-ANT-CRED    PIC X(020).
                 07 BVVEFC-T-VLR-PRE-CONTR        PIC X(020).
                 07 BVVEFC-T-VLR-ONUS-RESER       PIC X(020).
                 07 BVVEFC-T-IND-DOMC             PIC X(001).
                 07 BVVEFC-T-DOMCL-BANC.
                    09 BVVEFC-T-CNPJ-TIT-CTA        PIC 9(014).
                    09 BVVEFC-T-TP-CTA              PIC X(002).
                    09 BVVEFC-T-AGENCIA.
                       11 BVVEFC-T-AGENCIA-N        PIC 9(004).
                    09 BVVEFC-T-CONTA.
                       11 BVVEFC-T-CONTA-N          PIC 9(013).
                    09 BVVEFC-T-CTA-PGTO            PIC X(020).
                    09 BVVEFC-T-DT-EFET-PGTO        PIC X(008).
                    09 BVVEFC-T-VLR-EFET            PIC X(020).
                    09 BVVEFC-T-VLR-LIVRE           PIC X(020).
                    09 BVVEFC-T-IND-REC-INST        PIC X(001).
                    09 BVVEFC-T-REC-INSTIT.
                       11 BVVEFC-T-ID-OPER-REC      PIC X(019).
                       11 BVVEFC-T-PRIORIDADE       PIC 9(009).
                       11 BVVEFC-T-CNPJ-REGIS       PIC 9(014).
                       11 BVVEFC-T-VLR-NEGOC        PIC X(020).
                       11 BVVEFC-T-VLR-PER-A-CONS   PIC X(020).
                       11 BVVEFC-T-INDR-REGR-DIV    PIC X(001).
                       11 BVVEFC-T-DT-FIM-OPER      PIC X(008).
                 07 BVVEFC-T-OUTR-INST.
                    09 BVVEFC-T-INDR-REGR-DIV-O     PIC X(001).
                    09 BVVEFC-T-VLR-NEGOC-O         PIC X(020).
                    09 BVVEFC-T-VLR-PER-A-CONS-O    PIC X(020).
                    09 BVVEFC-T-DT-FIM-OPER-O       PIC X(008).
                    09 BVVEFC-T-PRIORIDADE-O        PIC 9(009).
              05 FILLER                             PIC X(055).
           03 BVVEFC-SEQ-HEAD                       PIC 9(005).
           03 BVVEFC-DMOVTO-HEAD                    PIC 9(008).
      *
