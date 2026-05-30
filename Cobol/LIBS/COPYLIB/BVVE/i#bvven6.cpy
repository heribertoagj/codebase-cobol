      *----------------------------------------------------------------*
      *  I#BVVEN6 - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  AREA PARA GERACAO DO ARQUIVO ARRC022
      *  SOLICITACAO DE INCLUSAO DE OPERACAO DE CESSAO PARA CIP        *
      *----------------------------------------------------------------*

       01  BVVEN6-DADOS-FIXOS.
           03 BVVEN6-GRUPO-ATLZ-NEGC-REC.
              05 BVVEN6-CHAVE-GRP-ATLZ-NEGC PIC X(008) VALUE '{100000}'.
              05 BVVEN6-IDENTD-PART-PRIN    PIC X(008).
              05 BVVEN6-IDENTD-PART-ADM     PIC X(008).
      *
       01  BVVEN6-DADOS-OPERACAO.
           03 BVVEN6-GRUPO-NEGC-REC.
              05 BVVEN6-CHAVE-GRP-NEGC-REC  PIC X(008) VALUE '{110000}'.
              05 BVVEN6-ID-NEGOC-RECBVL     PIC X(019).
              05 BVVEN6-ID-OPER             PIC X(019).
              05 BVVEN6-INDR-TP-NEGC        PIC X(002).
              05 BVVEN6-DT-VENC-OP          PIC X(008).
              05 BVVEN6-VLR-TOT-LIM-SLD     PIC X(020).
              05 BVVEN6-VLR-GAR             PIC X(020).
              05 BVVEN6-IND-GEST-ER         PIC X(001).
              05 BVVEN6-IND-REGR-DIVS       PIC X(001).
              05 BVVEN6-IND-ALC-CONTR-CRED  PIC X(001).
              05 BVVEN6-IND-ACTE-INCOND     PIC X(001).
              05 BVVEN6-IND-ACTE-UR-RESER   PIC X(001).
              05 BVVEN6-IND-I-A             PIC X(001).
              05 BVVEN6-ID-OP-DESCSTC       PIC X(019).
              05 BVVEN6-IND-AUT-CESS        PIC X(001).
              05 BVVEN6-GRUPO-CES-AUTD.
                 07 BVVEN6-CHAVE-GRP-CES-AU PIC X(008) VALUE '{111000}'.
                 07 BVVEN6-CNPJ-CPF-CES     PIC X(014).
              05 BVVEN6-GRUPO-RENEG-DIV.
                 07 BVVEN6-CHAVE-GRP-RENEG  PIC X(008) VALUE '{112000}'.
                 07 BVVEN6-ID-OPER-ORIG     PIC X(019).
      *
       01  BVVEN6-GRUPO-GEST-ER.
           03  BVVEN6-CHAVE-GRP-GEST-ER     PIC X(008) VALUE '{113000}'.

       01  BVVEN6-DADOS-PRODUTO.
           03 BVVEN6-GRUPO-TITULAR.
              05 BVVEN6-CHAVE-GRP-TIT       PIC X(008) VALUE '{113100}'.
              05 BVVEN6-CNPJ-OU-BASE-TITU   PIC X(014).
              05 BVVEN6-VLR-PERC-OPER       PIC X(020).
              05 BVVEN6-DT-INI-OPER         PIC X(008).
              05 BVVEN6-DT-FIM-OPER         PIC X(008).
              05 BVVEN6-CNPJ-CPF-TIT-CONTA  PIC X(014).
              05 BVVEN6-ISPB-BCO-RECBDR     PIC X(008).
              05 BVVEN6-TP-CTA              PIC X(002).
              05 BVVEN6-AGENCIA             PIC 9(004).
              05 BVVEN6-CONTA.
                 07 BVVEN6-NUM-CTA          PIC 9(012).
                 07 BVVEN6-DIG-CTA          PIC X(001).
              05 BVVEN6-CONTA-PGTO          PIC X(020).
           03 BVVEN6-GRUPO-CREDDR.
              05 BVVEN6-CHAVE-GRP-CRED      PIC X(008) VALUE '{113110}'.
              05 BVVEN6-CNPJ-CREDEN         PIC X(014).
           03 BVVEN6-GRUPO-ARRANJ-PGTO.
              05 BVVEN6-CHAVE-GRP-ARR-PGTO  PIC X(008) VALUE '{113120}'.
              05 BVVEN6-ARRANJ-PGTO         PIC X(003).
           03 BVVEN6-GRUPO-USUAR-FNAL-REC.
              05 BVVEN6-CHAVE-GRP-USUAR     PIC X(008) VALUE '{113130}'.
              05 BVVEN6-CNPJ-USUAR-FNAL     PIC X(014).

       01  BVVEN6-GESTAO-PART.
           03 BVVEN6-GRUPO-GEST-PART        PIC X(008) VALUE '{114000}'.
           03 BVVEN6-GRUPO-TIT-PART.
              05 BVVEN6-CHAVE-GRP-TIT-P     PIC X(008) VALUE '{114100}'.
              05 BVVEN6-CNPJ-OU-BASE-TIT-P  PIC X(014).

           03 BVVEN6-GRUPO-DOMCL-BANC.
              05 BVVEN6-CHAVE-GRP-DOCML     PIC X(008) VALUE '{114110}'.
              05 BVVEN6-CNPJ-CPF-TIT-CONTAP PIC X(014).
              05 BVVEN6-ISPB-BCO-RECBD-P    PIC X(008).
              05 BVVEN6-TP-CTA-P            PIC X(002).
              05 BVVEN6-AGENCIA-P           PIC 9(004).
              05 BVVEN6-CONTA-P.
                 07 BVVEN6-NUM-CTA-P        PIC 9(012).
                 07 BVVEN6-DIG-CTA-P        PIC X(001).
              05 BVVEN6-CONTA-PGTO-P        PIC X(020).

       01 BVVEN6-GRUPO-UR.
          03 BVVEN6-CHAVE-GRP-UR         PIC X(008) VALUE '{114111}'.
          03 BVVEN6-CNPJ-CREDEN-P        PIC X(014).
          03 BVVEN6-CNPJ-USUAR-UR        PIC X(014).
          03 BVVEN6-CNPJ-TIT-UR          PIC X(014).
          03 BVVEN6-ARRANJ-PGTO-UR       PIC X(003).
          03 BVVEN6-DT-LIQ-UR            PIC X(008).
          03 BVVEN6-VLR-PERC-UR          PIC X(020).

      *    DATA: FORMATO AAAAMMDD.
      *    VALOR: ATE DEZESSETE INTEIROS E DOIS DECIMAIS E SINALIZADO
