      *----------------------------------------------------------------*
      *  I#BVVENW - NOVOS NORMATIVOS (RESOLUCAO 4734)                  *
      *  CLIENTES DESPRAZADOS NA CARGA DA AGENDA FINANCEIRA
      *  TAMANHO: 210                                                  *
      *----------------------------------------------------------------*

       01  BVVENW-REGISTRO.
           03 BVVENW-COD-CREDENC                  PIC 9(004).
           03 BVVENW-CBAND-VISAO                  PIC 9(003).
           03 BVVENW-TP-PSSOA                     PIC X(001).
           03 BVVENW-CNPJ-CPF-TIT                 PIC 9(014).
           03 BVVENW-AGENCIA                      PIC 9(005).
           03 BVVENW-CONTA                        PIC 9(013).
           03 BVVENW-DATA-UR                      PIC X(010).
           03 BVVENW-VLR-DISPON                   PIC 9(015)V99.
           03 BVVENW-CNPJ-CPF-USUF                PIC 9(014).
           03 BVVENW-CNPJ-CREDENC                 PIC 9(014).
           03 BVVENW-ARRANJO                      PIC X(003).
           03 BVVENW-IND-DOMC                     PIC X(001).
           03 BVVENW-TP-CTA                       PIC X(002).
           03 BVVENW-COD-MOTIVO                   PIC 9(001).
           03 BVVENW-DES-MOTIVO                   PIC X(050).
           03 FILLER                              PIC X(058).
