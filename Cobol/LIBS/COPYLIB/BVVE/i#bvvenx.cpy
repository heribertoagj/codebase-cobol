      *================================================================*
      *  I#BVVENX - RETORNO DE CANCELAMENTO DA CIP                     *
      *  GERADO A PARTIR DO ARRC023RET                                 *
      *  LRECL 280 BYTES                                               *
      *----------------------------------------------------------------*
      *  BVVENX-SITUACAO  : 'A' ACEITO  OU 'R' REJEITADO               *
      *  BVVENX-DT-PRV-LIQ: AAAAMMDD                                   *
      *================================================================*

       01  BVVENX-RET-ARRC023.
           05 BVVENX-SITUACAO             PIC X(001).
           05 BVVENX-ID-OPER-BRAD         PIC X(019).
           05 BVVENX-ID-OPER-CIP          PIC X(019).
           05 BVVENX-ID-CANC-CIP          PIC X(019).
           05 BVVENX-CNPJ-BASE-TIT        PIC 9(014).
           05 BVVENX-IND-CANC-TOTAL       PIC X(001).
           05 BVVENX-IND-LIQD             PIC X(001).
           05 BVVENX-IND-A-CONST          PIC X(001).
           05 BVVENX-CNPJ-CREDENC         PIC 9(014).
           05 BVVENX-TP-PSSOA-USUAR       PIC X(001).
           05 BVVENX-CNPJ-USUAR           PIC 9(014).
           05 BVVENX-CPF-USUAR        REDEFINES BVVENX-CNPJ-USUAR.
              10 BVVENX-CPF-NUM           PIC 9(011).
              10 FILLER                   PIC X(003).
           05 BVVENX-ARRANJO              PIC X(003).
           05 BVVENX-DT-PRV-LIQ           PIC X(008).
           05 BVVENX-CNPJ-TIT-NEG         PIC 9(014).
           05 BVVENX-VLR-NEGC-CANC        PIC X(020).
           05 BVVENX-VLR-ACONST-CANC      PIC X(020).
           05 BVVENX-MOTIVO-REJEI         PIC X(050).
           05 BVVENX-COD-CREDENC          PIC 9(004).
           05 BVVENX-BAND-BVVE            PIC 9(003).
           05 BVVENX-PRODUTO              PIC 9(005).
           05 BVVENX-ID-OPER-GRAN         PIC X(019).
           05 BVVENX-AGENCIA              PIC 9(005).
           05 BVVENX-CONTA                PIC 9(013).
           05 FILLER                      PIC X(012).
