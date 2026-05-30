      *----------------------------------------------------------------*
      *  I#BVVEN7 - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  AREA PARA EXTRACAO DO ARQUIVO ARRC022RET
      *  RETORNO DE INCLUSAO DE OPERACAO NA CIP                        *
      *----------------------------------------------------------------*

       01  BVVEN7-GRUPO-ATLZ-NEGC-REC.
           03 BVVEN7-CHAVE-GRP-ATLZ-NEGC    PIC X(008) VALUE '{100000}'.
           03 BVVEN7-DADOS-GRP-ATLZ-NEGC.
              05 FILLER                      PIC X(008).
              05 BVVEN7-SIT-RET-REQ          PIC X(003).
              05 BVVEN7-SIT-RET-REQ-ER       PIC X(008).
              05 BVVEN7-IDENTD-PART-PRIN     PIC X(008).
              05 BVVEN7-IDENTD-PART-PRIN-ERR PIC X(008).
              05 BVVEN7-IDENTD-PART-ADM      PIC X(008).
              05 BVVEN7-IDENTD-PART-ADM-ERR  PIC X(008).
      *
       01  BVVEN7-GRUPO-NEGC-ACEITO.
           03 BVVEN7-CHAVE-GRP-ACEITO       PIC X(008) VALUE '{110000}'.
           03 BVVEN7-DADOS-GRP-ACEITO.
              05 FILLER                        PIC X(008).
              05 BVVEN7-CTRL-NEGOC-RECBVL-AC   PIC X(040).
              05 BVVEN7-CTRL-NEGOC-REC-AC-ERR  PIC X(008).
              05 BVVEN7-ID-OPER-AC             PIC X(019).
              05 BVVEN7-ID-OPER-AC-ERR         PIC X(008).
      *
       01  BVVEN7-GRUPO-RECBVL.
           03 BVVEN7-CHAVE-GRP-RECBVL       PIC X(008) VALUE '{111000}'.
           03 BVVEN7-DADOS-GRP-RECBVL.
              05 FILLER                       PIC X(008).
              05 BVVEN7-PRIOR-NEGC-REC-AC     PIC 9(009).
              05 BVVEN7-PRIOR-NEGC-REC-AC-E   PIC X(008).
              05 BVVEN7-CNPJ-CPF-USUFIN-AC    PIC 9(014).
              05 BVVEN7-CNPJ-CPF-USUFIN-AC-E  PIC X(008).
              05 BVVEN7-CNPJ-CPF-TIT-AC       PIC X(014).
              05 BVVEN7-CNPJ-CPF-TIT-AC-E     PIC X(008).
              05 BVVEN7-CNPJ-CREDENC-AC       PIC 9(014).
              05 BVVEN7-CNPJ-CREDENC-AC-E     PIC X(008).
              05 BVVEN7-ARR-PGTO-AC           PIC X(003).
              05 BVVEN7-ARR-PGTO-AC-E         PIC X(008).
              05 BVVEN7-DT-LIQ-AC             PIC X(008).
              05 BVVEN7-DT-LIQ-AC-E           PIC X(008).
              05 BVVEN7-VLR-NEG-AC            PIC X(020).
              05 BVVEN7-VLR-NEG-AC-E          PIC X(008).
      *
       01  BVVEN7-GRUPO-REG-CONSTITR.
           03 BVVEN7-CHAVE-GRP-CONSTITR     PIC X(008) VALUE '{112000}'.
           03 BVVEN7-DADOS-GRP-CONSTITR.
              05 FILLER                        PIC X(008).
              05 BVVEN7-PRIOR-NEGC-REC-CONS    PIC 9(009).
              05 BVVEN7-PRIOR-NEGC-REC-CONS-E  PIC X(008).
              05 BVVEN7-CNPJ-CPF-USUFIN-CONS   PIC 9(014).
              05 BVVEN7-CNPJ-CPF-USUFIN-CONS-E PIC X(008).
              05 BVVEN7-CNPJ-CPF-TIT-CONS      PIC X(014).
              05 BVVEN7-CNPJ-CPF-TIT-CONS-E    PIC X(008).
              05 BVVEN7-CNPJ-CREDENC-CONS      PIC 9(014).
              05 BVVEN7-CNPJ-CREDENC-CONS-E    PIC X(008).
              05 BVVEN7-ARR-PGTO-CONS          PIC X(003).
              05 BVVEN7-ARR-PGTO-CONS-E        PIC X(008).
              05 BVVEN7-DT-LIQ-CONS            PIC X(008).
              05 BVVEN7-DT-LIQ-CONS-E          PIC X(008).
              05 BVVEN7-IND-REGRA-DIVIS-CONS   PIC X(001).
              05 BVVEN7-IND-REGRA-DIVIS-CONS-E PIC X(008).
              05 BVVEN7-VLR-NEG-CONS           PIC X(020).
              05 BVVEN7-VLR-NEG-CONS-E         PIC X(008).
      *
       01  BVVEN7-GRUPO-REG-RECUSADO.
           03 BVVEN7-CHAVE-GRP-RECUS        PIC X(008) VALUE '{120000}'.
           03 BVVEN7-DADOS-GRP-RECUS.
              05 FILLER                        PIC X(008).
              05 BVVEN7-ID-NEGOC-REC-REC       PIC X(040).
              05 BVVEN7-ID-NEGOC-REC-REC-E     PIC X(008).
              05 BVVEN7-ID-OPER-REC            PIC X(019).
              05 BVVEN7-ID-OPER-REC-E          PIC X(008).
              05 BVVEN7-INDR-TP-NEGC-REC       PIC X(002).
              05 BVVEN7-INDR-TP-NEGC-E         PIC X(008).
              05 BVVEN7-DT-VENC-OP-REC         PIC X(008).
              05 BVVEN7-DT-VENC-OP-REC-E       PIC X(008).
              05 BVVEN7-VLR-TOT-LIM-REC        PIC X(020).
              05 BVVEN7-VLR-TOT-LIM-REC-E      PIC X(008).
              05 BVVEN7-VLR-GAR-REC            PIC X(020).
              05 BVVEN7-VLR-GAR-REC-E          PIC X(008).
              05 BVVEN7-IND-GEST-ER-REC        PIC X(001).
              05 BVVEN7-IND-GEST-ER-REC-E      PIC X(008).
              05 BVVEN7-IND-REGR-DIVS-REC      PIC X(001).
              05 BVVEN7-IND-REGR-DIVS-REC-E    PIC X(008).
              05 BVVEN7-IND-ALC-CONTR-CRED-REC PIC X(001).
              05 BVVEN7-IND-ALC-CONTR-CRED-E   PIC X(008).
              05 BVVEN7-IND-ACTE-INCOND-REC    PIC X(001).
              05 BVVEN7-IND-ACTE-INCOND-REC-E  PIC X(008).
              05 BVVEN7-IND-ACTE-UR-RESER-REC  PIC X(001).
              05 BVVEN7-IND-ACTE-UR-RESER-E    PIC X(008).
              05 BVVEN7-IND-I-A-REC            PIC X(001).
              05 BVVEN7-IND-I-A-REC-E          PIC X(008).
              05 BVVEN7-ID-OP-DESCSTC-REC      PIC X(019).
              05 BVVEN7-ID-OP-DESCSTC-REC-E    PIC X(008).
              05 BVVEN7-IND-AUT-CESS-REC       PIC X(001).
              05 BVVEN7-IND-AUT-CESS-REC-E     PIC X(008).
      *
       01  BVVEN7-GRUPO-CES-AUTD.
           03 BVVEN7-CHAVE-GRP-CES-AUTD     PIC X(008) VALUE '{121000}'.
           03 BVVEN7-DADOS-GRP-CES-AUTD.
              05 FILLER                     PIC X(008).
              05 BVVEN7-CNPJ-CPF-CES-AUTD   PIC X(014).
              05 BVVEN7-CNPJ-CPF-CES-AUTD-E PIC X(008).
      *
       01  BVVEN7-GRUPO-RENEG-DIV.
           03 BVVEN7-CHAVE-GRP-RENEG-DIV    PIC X(008) VALUE '{122000}'.
           03 BVVEN7-DADOS-GRP-RENEG-DIV.
              05 FILLER                     PIC X(008).
              05 BVVEN7-ID-OPER-RENEG-DIV   PIC X(019).
              05 BVVEN7-ID-OPER-RENEG-DIV-E PIC X(008).
      *
       01  BVVEN7-GRUPO-GESTAO-ER.
           03 BVVEN7-CHAVE-GRP-GESTAO-ER    PIC X(008) VALUE '{123000}'.
           03 BVVEN7-DADOS-GRP-GESTAO-ER    PIC X(008).
      *
       01  BVVEN7-GRUPO-TITULAR.
           03 BVVEN7-CHAVE-GRP-TIT          PIC X(008) VALUE '{123100}'.
           03 BVVEN7-DADOS-GRP-TIT.
              05 FILLER                        PIC X(008).
              05 BVVEN7-CNPJ-OU-BASE-TITULAR   PIC X(014).
              05 BVVEN7-CNPJ-OU-BASE-TITULAR-E PIC X(008).
              05 BVVEN7-VLR-PERC-OPER          PIC X(020).
              05 BVVEN7-VLR-PERC-OPER-E        PIC X(008).
              05 BVVEN7-DT-INI-OPER            PIC X(008).
              05 BVVEN7-DT-INI-OPER-E          PIC X(008).
              05 BVVEN7-DT-FIM-OPER            PIC X(008).
              05 BVVEN7-DT-FIM-OPER-E          PIC X(008).
              05 BVVEN7-CNPJ-CPF-TIT-CONTA     PIC X(014).
              05 BVVEN7-CNPJ-CPF-TIT-CONTA-E   PIC X(008).
              05 BVVEN7-ISPB-BCO-RECBDR        PIC 9(008).
              05 BVVEN7-ISPB-BCO-RECBDR-E      PIC X(008).
              05 BVVEN7-TP-CTA                 PIC X(002).
              05 BVVEN7-TP-CTA-E               PIC X(008).
              05 BVVEN7-AGENCIA                PIC 9(004).
              05 BVVEN7-AGENCIA-E              PIC X(008).
              05 BVVEN7-CONTA.
                 07 BVVEN7-NUM-CTA             PIC 9(012).
                 07 BVVEN7-DIG-CTA             PIC 9(001).
              05 BVVEN7-CONTA-E                PIC X(008).
              05 BVVEN7-CONTA-PGTO             PIC X(020).
              05 BVVEN7-CONTA-PGTO-E           PIC X(008).
      *
       01  BVVEN7-GRUPO-CREDDR.
           03 BVVEN7-CHAVE-GRP-CRED         PIC X(008) VALUE '{123110}'.
           03 BVVEN7-DADOS-GRP-CRED.
              05 FILLER                     PIC X(008).
              05 BVVEN7-CNPJ-CREDEN         PIC X(014).
              05 BVVEN7-CNPJ-CREDEN-E       PIC X(008).
      *
       01  BVVEN7-GRUPO-ARRANJ-PGTO.
           03 BVVEN7-CHAVE-GRP-ARR-PGTO     PIC X(008) VALUE '{123120}'.
           03 BVVEN7-DADOS-GRP-ARR-PGTO.
              05 FILLER                     PIC X(008).
              05 BVVEN7-ARRANJ-PGTO         PIC X(003).
              05 BVVEN7-ARRANJ-PGTO-E       PIC X(008).
      *
       01  BVVEN7-GRUPO-USUAR-FNAL-REC.
           03 BVVEN7-CHAVE-GRP-USUAR        PIC X(008) VALUE '{123130}'.
           03 BVVEN7-DADOS-GRP-USUAR.
              05 FILLER                     PIC X(008).
              05 BVVEN7-CNPJ-USUAR-FNAL     PIC 9(014).
              05 BVVEN7-CNPJ-USUAR-FNAL-E   PIC X(008).
      *
       01  BVVEN7-GRUPO-GESTAO-PARTICIP.
           03 BVVEN7-CHAVE-GRP-GESTAO-PART  PIC X(008) VALUE '{124000}'.
           03 BVVEN7-DADOS-GRP-GESTAO-PART  PIC X(008).
      *
       01  BVVEN7-GRUPO-TITULAR-PART.
           03 BVVEN7-CHAVE-GRP-TIT-PART     PIC X(008) VALUE '{124100}'.
           03 BVVEN7-DADOS-GRP-TIT-PART.
              05 FILLER                        PIC X(008).
              05 BVVEN7-CNPJ-OU-BASE-TIT-PART  PIC 9(014).
              05 BVVEN7-CNPJ-OU-BASE-TIT-PAR-E PIC X(008).
      *
       01  BVVEN7-GRUPO-DOMCL-BANC.
           03 BVVEN7-CHAVE-GRP-DOMC-BANC    PIC X(008) VALUE '{124110}'.
           03 BVVEN7-DADOS-GRP-DOMC-BANC.
              05 FILLER                        PIC X(008).
              05 BVVEN7-CNPJ-OU-BASE-TIT-DOM   PIC X(014).
              05 BVVEN7-CNPJ-OU-BASE-TIT-DOM-E PIC X(008).
              05 BVVEN7-ISPB-BCO-RECB-DOM      PIC 9(008).
              05 BVVEN7-ISPB-BCO-RECB-DOM-E    PIC X(008).
              05 BVVEN7-TP-CTA-DOM             PIC X(002).
              05 BVVEN7-TP-CTA-DOM-E           PIC X(008).
              05 BVVEN7-AGENCIA-DOM            PIC 9(004).
              05 BVVEN7-AGENCIA-DOM-E          PIC X(008).
              05 BVVEN7-CONTA-DOM.
                 07 BVVEN7-NUM-CTA-DOM         PIC 9(012).
                 07 BVVEN7-DIG-CTA-DOM         PIC 9(001).
              05 BVVEN7-CONTA-DOM-E            PIC X(008).
              05 BVVEN7-CONTA-PGTO-DOM         PIC X(020).
              05 BVVEN7-CONTA-PGTO-DOM-E       PIC X(008).
      *
       01  BVVEN7-GRUPO-REG-RECBL.
           03 BVVEN7-CHAVE-GRP-RECB         PIC X(008) VALUE '{124111}'.
           03 BVVEN7-DADOS-GRP-RECB.
              05 FILLER                       PIC X(008).
              05 BVVEN7-CNPJ-CREDE-REC-REJ    PIC 9(014).
              05 BVVEN7-CNPJ-CREDE-REC-REJ-E  PIC X(008).
              05 BVVEN7-CNPJ-USU-REC-REJ      PIC X(014).
              05 BVVEN7-CNPJ-USU-REC-REJ-E    PIC X(008).
              05 BVVEN7-CNPJ-TIT-REC-REJ      PIC X(014).
              05 BVVEN7-CNPJ-TIT-REC-REJ-E    PIC X(008).
              05 BVVEN7-ARRANJ-PGTO-REC-REJ   PIC X(003).
              05 BVVEN7-ARRANJ-PGTO-REC-REJ-E PIC X(008).
              05 BVVEN7-DT-LIQ-REC-REJ        PIC X(008).
              05 BVVEN7-DT-LIQ-REC-REJ-E      PIC X(008).
              05 BVVEN7-VLR-REC-REJ           PIC X(020).
              05 BVVEN7-VLR-REC-REJ-E         PIC X(008).

      *    DATA: FORMATO AAAA-MM-DD.
      *    VALOR: ATE DEZESSETE INTEIROS E DOIS DECIMAIS E SINALIZADO
      *    SE NEGATIVO. DECIMAIS SEPARADOS POR PONTOS.
