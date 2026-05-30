      *----------------------------------------------------------------*
      *  I#BVVEFI - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  AREA PARA GERACAO DO ARQUIVO ARRC023
      *  SOLICITACAO CANCELAMENTO DE OPERACAO NA CIP                   *
      *----------------------------------------------------------------*

       01  BVVEFI-GRUPO-CANCEL-NEGC.
           03 BVVEFI-CHAVE-GRP-ATLZ-NEGC    PIC X(008) VALUE '{100000}'.
           03 BVVEFI-IDENTD-PART-PRIN       PIC X(008).
           03 BVVEFI-IDENTD-PART-ADM        PIC X(008).
      *
       01  BVVEFI-GRUPO-NEGC-REC.
           03 BVVEFI-CHAVE-GRP-NEGC-REC     PIC X(008) VALUE '{110000}'.
           03 BVVEFI-ID-NEGOC-RECBVL        PIC X(019).
           03 BVVEFI-ID-OPER                PIC X(019).
           03 BVVEFI-CNPJ-OU-BASE-TITU      PIC X(014).
           03 BVVEFI-INDR-CANC-TOTAL        PIC X(001).
           03 BVVEFI-INDR-LIQ-OPER          PIC X(001).
           03 BVVEFI-IND-CANC-A-CONST       PIC X(001).
       01  BVVEFI-GRUPO-REG-RECBVL.
           05 BVVEFI-CHAVE-GRP-REG-RECB     PIC X(008) VALUE '{111000}'.
           05 BVVEFI-CNPJ-CREDENC           PIC 9(014).
           05 BVVEFI-CNPJ-USU-FNAL          PIC 9(014).
           05 BVVEFI-ARRANJ-PGTO            PIC X(003).
           05 BVVEFI-DT-PREV-LIQ            PIC X(008).
           05 BVVEFI-CNPJ-CPF-TIT-VENDEDOR  PIC 9(014).
           05 BVVEFI-VLR-CANCE              PIC X(020).
           05 BVVEFI-VLR-A-CONS-CANCE       PIC X(020).
