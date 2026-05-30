      *================================================================*
      *  I#BVVENV - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  AREA PARA EXTRACAO DO ARQUIVO ARRC023RET                      *
      *  RETORNO DE CANCELAMENTO DE OPERACAO NA CIP                    *
      *----------------------------------------------------------------*
      *  DATA : AAAAMMDD                                               *
      *  VALOR: ATE DEZESSETE INTEIROS E DOIS DECIMAIS + SINAL         *
      *         SE NEGATIVO. DECIMAIS SEPARADOS POR PONTOS.            *
      *================================================================*

       01  BVVENV-GRUPO-CANC-NEGC-REC.
           03 BVVENV-CHAVE-GRP-CANC-NEGC    PIC X(008) VALUE '{100000}'.
           03 BVVENV-DADOS-GRP-CANC-NEGC.
              05 FILLER                      PIC X(008).
              05 BVVENV-SIT-RET-REQ          PIC X(003).
              05 BVVENV-SIT-RET-REQ-E        PIC X(008).
              05 BVVENV-IDENTD-PART-PRIN     PIC X(008).
              05 BVVENV-IDENTD-PART-PRIN-E   PIC X(008).
              05 BVVENV-IDENTD-PART-ADM      PIC X(008).
              05 BVVENV-IDENTD-PART-ADM-E    PIC X(008).
      *
       01  BVVENV-GRUPO-NEGC-ACEITO.
           03 BVVENV-CHAVE-GRP-ACEITO       PIC X(008) VALUE '{110000}'.
           03 BVVENV-DADOS-GRP-ACEITO.
              05 FILLER                        PIC X(008).
              05 BVVENV-ID-NEGC-RECBVL-AC      PIC X(040).
              05 BVVENV-ID-NEGC-RECBVL-AC-E    PIC X(008).
              05 BVVENV-ID-OPER-AC             PIC X(019).
              05 BVVENV-ID-OPER-AC-E           PIC X(008).
              05 BVVENV-ID-CANC-AC             PIC X(019).
              05 BVVENV-ID-CANC-AC-E           PIC X(008).
      *
       01  BVVENV-GRUPO-NEGC-REJEITADO.
           03 BVVENV-CHAVE-GRP-REJEIT       PIC X(008) VALUE '{120000}'.
           03 BVVENV-DADOS-GRP-REJEIT.
              05 FILLER                        PIC X(008).
              05 BVVENV-ID-NEGC-RECBVL-REJ     PIC X(040).
              05 BVVENV-ID-NEGC-RECBVL-REJ-E   PIC X(008).
              05 BVVENV-ID-OPER-REJ            PIC X(019).
              05 BVVENV-ID-OPER-REJ-E          PIC X(008).
              05 BVVENV-CNPJ-BASE-TITU-REJ     PIC X(014).
              05 BVVENV-CNPJ-BASE-TITU-REJ-E   PIC X(008).
              05 BVVENV-IND-CANC-VLTOT-REJ     PIC X(001).
              05 BVVENV-IND-CANC-VLTOT-REJ-E   PIC X(008).
              05 BVVENV-IND-LIQ-OPER-REJ       PIC X(001).
              05 BVVENV-IND-LIQ-OPER-REJ-E     PIC X(008).
              05 BVVENV-IND-A-CONST-REJ        PIC X(001).
              05 BVVENV-IND-A-CONST-REJ-E      PIC X(008).

       01  BVVENV-GRUPO-REG-RECEBIVEL.
           03 BVVENV-CHAVE-GRP-REG-RECBVL   PIC X(008) VALUE '{121000}'.
           03 BVVENV-DADOS-GRP-REG-RECBVL.
              05 FILLER                        PIC X(008).
              05 BVVENV-RCB-CNPJ-CRED          PIC X(014).
              05 BVVENV-RCB-CNPJ-CRED-E        PIC X(008).
              05 BVVENV-RCB-CNPJ-USER          PIC X(014).
              05 BVVENV-RCB-CNPJ-USER-E        PIC X(008).
              05 BVVENV-RCB-ARRANJO            PIC X(003).
              05 BVVENV-RCB-ARRANJO-E          PIC X(008).
              05 BVVENV-RCB-DT-PRV-LIQ         PIC X(008).
              05 BVVENV-RCB-DT-PRV-LIQ-E       PIC X(008).
              05 BVVENV-RCB-CNPJ-TIT-NEG       PIC X(014).
              05 BVVENV-RCB-CNPJ-TIT-NEG-E     PIC X(008).
              05 BVVENV-RCB-VLR-NEGC-CANC      PIC X(020).
              05 BVVENV-RCB-VLR-NEGC-CANC-E    PIC X(008).
              05 BVVENV-RCB-VLR-ACONST-CANC    PIC X(020).
              05 BVVENV-RCB-VLR-ACONST-CANC-E  PIC X(008).
