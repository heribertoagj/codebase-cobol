      *----------------------------------------------------------------*
      *  I#BVVEFB - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  AREA PARA EXTRACAO DO ARQUIVO ARRC018
      *  AGENDA FINANCEIRA CIP                                         *
      *----------------------------------------------------------------*

       01  BVVEFB-INICIO-FIXO.
           03 BVVEFB-IDENTD-PART-PRIN       PIC X(008).
           03 BVVEFB-IDENTD-PART-PRIN-E     PIC X(008).
           03 BVVEFB-IDENTD-PART-ADM        PIC X(008).
           03 BVVEFB-IDENTD-PART-ADM-E      PIC X(008).
           03 BVVEFB-CNPJ-REGSTRAD          PIC 9(014).
           03 BVVEFB-CNPJ-REGSTRAD-E        PIC X(008).
           03 BVVEFB-CNPJ-CREDENC           PIC 9(014).
           03 BVVEFB-CNPJ-CREDENC-E         PIC X(008).
      *
       01  BVVEFB-GRUPO1-ARR-PGTO.
           03 FILLER                        PIC X(008) VALUE '{100000}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G1-ARR-PGTO            PIC X(003).
           03 BVVEFB-G1-ARR-PGTO-E          PIC X(008).
      *
       01  BVVEFB-GRUPO2-USU-FNAL.
           03 FILLER                        PIC X(008) VALUE '{110000}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G2-CNPJ-CPF-USUFIN     PIC X(014).
           03 BVVEFB-G2-CNPJ-CPF-USUFIN-E   PIC X(008).
           03 BVVEFB-G2-VLR-LIV-USU-FNAL    PIC X(020).
           03 BVVEFB-G2-VLR-LIV-USU-FNAL-E  PIC X(008).
      *
       01  BVVEFB-GRUPO3-UNID-RECEB.
           03 FILLER                        PIC X(008) VALUE '{111000}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G3-DT-PREV-LIQ         PIC X(008).
           03 BVVEFB-G3-DT-PREV-LIQ-E       PIC X(008).
           03 BVVEFB-G3-VLR-TOT             PIC X(020).
           03 BVVEFB-G3-VLR-TOT-E           PIC X(008).
           03 BVVEFB-G3-IND-DOMCL           PIC X(001).
           03 BVVEFB-G3-IND-DOMCL-E         PIC X(008).
      *
       01  BVVEFB-GRUPO4-TITULAR.
           03 FILLER                        PIC X(008) VALUE '{111100}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G4-CNPJ-CPF-TIT        PIC X(014).
           03 BVVEFB-G4-CNPJ-CPF-TIT-E      PIC X(008).
           03 BVVEFB-G4-VLR-TOT-TITU        PIC X(020).
           03 BVVEFB-G4-VLR-TOT-TITU-E      PIC X(008).
           03 BVVEFB-G4-VLR-COMP-OUTRA      PIC X(020).
           03 BVVEFB-G4-VLR-COMP-OUTRA-E    PIC X(008).
           03 BVVEFB-G4-VLR-COMP-INST       PIC X(020).
           03 BVVEFB-G4-VLR-COMP-INST-E     PIC X(008).
           03 BVVEFB-G4-VLR-LIVR-TOT        PIC X(020).
           03 BVVEFB-G4-VLR-LIVR-TOT-E      PIC X(008).
           03 BVVEFB-G4-VLR-LIVR-ANT-CRED   PIC X(020).
           03 BVVEFB-G4-VLR-LIVR-ANT-CRED-E PIC X(008).
           03 BVVEFB-G4-VLR-PRE-CONTR       PIC X(020).
           03 BVVEFB-G4-VLR-PRE-CONTR-E     PIC X(008).
           03 BVVEFB-G4-VLR-ONUS-RESER      PIC X(020).
           03 BVVEFB-G4-VLR-ONUS-RESER-E    PIC X(008).
      *
       01  BVVEFB-GRUPO5-DOMCL-BANC.
           03 FILLER                        PIC X(008) VALUE '{111110}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G5-CNPJ-TIT-CTA        PIC 9(014).
           03 BVVEFB-G5-CNPJ-TIT-CTA-E      PIC X(008).
           03 BVVEFB-G5-TP-CTA              PIC X(002).
           03 BVVEFB-G5-TP-CTA-E            PIC X(008).
           03 BVVEFB-G5-AGENCIA             PIC X(004).
           03 BVVEFB-G5-AGENCIA-E           PIC X(008).
           03 BVVEFB-G5-CONTA               PIC X(013).
           03 BVVEFB-G5-CONTA-E             PIC X(008).
           03 BVVEFB-G5-CTA-PGTO            PIC X(020).
           03 BVVEFB-G5-CTA-PGTO-E          PIC X(008).
           03 BVVEFB-G5-DT-EFET-PGTO        PIC X(008).
           03 BVVEFB-G5-DT-EFET-PGTO-E      PIC X(008).
           03 BVVEFB-G5-VLR-EFET            PIC X(020).
           03 BVVEFB-G5-VLR-EFET-E          PIC X(008).
           03 BVVEFB-G5-VLR-LIVRE           PIC X(020).
           03 BVVEFB-G5-VLR-LIVRE-E         PIC X(008).
      *
       01  BVVEFB-GRUPO6-REC-INSTIT.
           03 FILLER                        PIC X(008) VALUE '{111111}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G6-ID-OPER             PIC X(019).
           03 BVVEFB-G6-ID-OPER-E           PIC X(008).
           03 BVVEFB-G6-PRIORIDADE          PIC 9(009).
           03 BVVEFB-G6-PRIORIDADE-E        PIC X(008).
           03 BVVEFB-G6-CNPJ-REGIS          PIC 9(014).
           03 BVVEFB-G6-CNPJ-REGIS-E        PIC X(008).
           03 BVVEFB-G6-VLR-NEGOC           PIC X(020).
           03 BVVEFB-G6-VLR-NEGOC-E         PIC X(008).
           03 BVVEFB-G6-VLR-A-CONS          PIC X(020).
           03 BVVEFB-G6-VLR-A-CONS-E        PIC X(008).
           03 BVVEFB-G6-INDR-REGR-DIV       PIC X(001).
           03 BVVEFB-G6-INDR-REGR-DIV-E     PIC X(008).
           03 BVVEFB-G6-DT-FIM-OPER         PIC X(008).
           03 BVVEFB-G6-DT-FIM-OPER-E       PIC X(008).
      *
       01  BVVEFB-GRUPO7-OUTR-INST.
           03 FILLER                        PIC X(008) VALUE '{111120}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G7-INDR-REGR-DIV       PIC X(001).
           03 BVVEFB-G7-INDR-REGR-DIV-E     PIC X(008).
           03 BVVEFB-G7-VLR-NEGOC           PIC X(020).
           03 BVVEFB-G7-VLR-NEGOC-E         PIC X(008).
           03 BVVEFB-G7-VLR-A-CONS          PIC X(020).
           03 BVVEFB-G7-VLR-A-CONS-E        PIC X(008).
           03 BVVEFB-G7-DT-FIM-OPER         PIC X(008).
           03 BVVEFB-G7-DT-FIM-OPER-E       PIC X(008).
           03 BVVEFB-G7-PRIORIDADE          PIC 9(009).
           03 BVVEFB-G7-PRIORIDADE-E        PIC X(008).
      *
       01  BVVEFB-GRUPO8-TITULAR.
           03 FILLER                        PIC X(008) VALUE '{120000}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G8-CNPJ-CPF-TITULAR    PIC 9(014).
           03 BVVEFB-G8-CNPJ-CPF-TITULAR-E  PIC X(008).
      *
       01  BVVEFB-GRUPO9-UNID-RECEB.
           03 FILLER                        PIC X(008) VALUE '{121000}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G9-DT-PREV-LIQ         PIC X(008).
           03 BVVEFB-G9-DT-PREV-LIQ-E       PIC X(008).
           03 BVVEFB-G9-VLR-TOT-TIT         PIC X(020).
           03 BVVEFB-G9-VLR-TOT-TIT-E       PIC X(008).
           03 BVVEFB-G9-VLR-COMP-OUTRA      PIC X(020).
           03 BVVEFB-G9-VLR-COMP-OUTRA-E    PIC X(008).
           03 BVVEFB-G9-VLR-COMP-INST       PIC X(020).
           03 BVVEFB-G9-VLR-COMP-INST-E     PIC X(008).
           03 BVVEFB-G9-VLR-LIVR-TOT        PIC X(020).
           03 BVVEFB-G9-VLR-LIVR-TOT-E      PIC X(008).
           03 BVVEFB-G9-VLR-LIVR-ANT-CRED   PIC X(020).
           03 BVVEFB-G9-VLR-LIVR-ANT-CRED-E PIC X(008).
           03 BVVEFB-G9-VLR-PRE-CONTR       PIC X(020).
           03 BVVEFB-G9-VLR-PRE-CONTR-E     PIC X(008).
           03 BVVEFB-G9-VLR-ONUS-RESER      PIC X(020).
           03 BVVEFB-G9-VLR-ONUS-RESER-E    PIC X(008).
      *
       01  BVVEFB-GRUPO10-DOMCL-BANC.
           03 FILLER                        PIC X(008) VALUE '{121100}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G10-CNPJ-TIT-CTA       PIC 9(014).
           03 BVVEFB-G10-CNPJ-TIT-CTA-E     PIC X(008).
           03 BVVEFB-G10-TP-CTA             PIC X(002).
           03 BVVEFB-G10-TP-CTA-E           PIC X(008).
           03 BVVEFB-G10-AGENCIA            PIC X(004).
           03 BVVEFB-G10-AGENCIA-E          PIC X(008).
           03 BVVEFB-G10-CONTA              PIC X(013).
           03 BVVEFB-G10-CONTA-E            PIC X(008).
           03 BVVEFB-G10-CTA-PGTO           PIC X(020).
           03 BVVEFB-G10-CTA-PGTO-E         PIC X(008).
           03 BVVEFB-G10-DT-EFET-PGTO       PIC X(008).
           03 BVVEFB-G10-DT-EFET-PGTO-E     PIC X(008).
           03 BVVEFB-G10-VLR-EFET           PIC X(020).
           03 BVVEFB-G10-VLR-EFET-E         PIC X(008).
           03 BVVEFB-G10-VLR-LIVRE          PIC X(020).
           03 BVVEFB-G10-VLR-LIVRE-E        PIC X(008).
      *
       01  BVVEFB-GRUPO11-REC-INSTIT.
           03 FILLER                        PIC X(008) VALUE '{121110}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G11-ID-OPER-REC        PIC X(019).
           03 BVVEFB-G11-ID-OPER-REC-E      PIC X(008).
           03 BVVEFB-G11-PRIORIDADE         PIC 9(009).
           03 BVVEFB-G11-PRIORIDADE-E       PIC X(008).
           03 BVVEFB-G11-CNPJ-REGIS         PIC 9(014).
           03 BVVEFB-G11-CNPJ-REGIS-E       PIC X(008).
           03 BVVEFB-G11-VLR-NEGOC          PIC X(020).
           03 BVVEFB-G11-VLR-NEGOC-E        PIC X(008).
           03 BVVEFB-G11-VLR-PER-A-CONS     PIC X(020).
           03 BVVEFB-G11-VLR-PER-A-CONS-E   PIC X(008).
           03 BVVEFB-G11-INDR-REGR-DIV      PIC X(001).
           03 BVVEFB-G11-INDR-REGR-DIV-E    PIC X(008).
           03 BVVEFB-G11-DT-FIM-OPER        PIC X(008).
           03 BVVEFB-G11-DT-FIM-OPER-E      PIC X(008).
      *
       01  BVVEFB-GRUPO12-OUTR-INST.
           03 FILLER                        PIC X(008) VALUE '{121200}'.
           03 FILLER                        PIC X(008).
           03 BVVEFB-G12-INDR-REGR-DIV      PIC X(001).
           03 BVVEFB-G12-INDR-REGR-DIV-E    PIC X(008).
           03 BVVEFB-G12-VLR-NEGOC          PIC X(020).
           03 BVVEFB-G12-VLR-NEGOC-E        PIC X(008).
           03 BVVEFB-G12-VLR-PER-A-CONS     PIC X(020).
           03 BVVEFB-G12-VLR-PER-A-CONS-E   PIC X(008).
           03 BVVEFB-G12-DT-FIM-OPER        PIC X(008).
           03 BVVEFB-G12-DT-FIM-OPER-E      PIC X(008).
           03 BVVEFB-G12-PRIORIDADE         PIC 9(009).
           03 BVVEFB-G12-PRIORIDADE-E       PIC X(008).
      *
