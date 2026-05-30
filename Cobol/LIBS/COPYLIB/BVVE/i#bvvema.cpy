      *----------------------------------------------------------------*
      *  I#BVVEMA - MANUTENCAO DE DOMICILIO BANCARIO - EM MASSA        *
      *  FORMATO DATAS: AAAAMMDD                                       *
      *----------------------------------------------------------------*

       01  BVVEMA-REGISTRO.
           03  BVVEMA-ROTINA                 PIC X(004).
           03  BVVEMA-ORIGEM                 PIC X(001).
           03  BVVEMA-DATA                   PIC 9(008).
           03  BVVEMA-SEQ                    PIC 9(003).
           03  BVVEMA-COD-MSG                PIC X(007).
           03  BVVEMA-NUMCTRL-IF-ENVIO       PIC X(020).
           03  BVVEMA-SCG0011.
               05  BVVEMA-I-DADOS-CLI.
                   07 BVVEMA-I-TP-PESSOA-CLI PIC X(001).
                   07 BVVEMA-I-CNPJ-CPF-CLI  PIC 9(014).
                   07 BVVEMA-I-CPF           REDEFINES
                      BVVEMA-I-CNPJ-CPF-CLI.
                      09 BVVEMA-I-ZEROS      PIC 9(003).
                      09 BVVEMA-I-CPF-NUM    PIC 9(011).
               05  BVVEMA-I-TRAVA-RAIZ       PIC X(001).
               05  BVVEMA-I-COD-PROPT-ESQM   PIC X(003).
               05  BVVEMA-I-TP-PRODT         PIC X(001).
               05  BVVEMA-I-NUM-COD-IF       PIC X(003).
               05  BVVEMA-I-AG-CLI           PIC 9(004).
      ***      CONTA POSSUI 13 BYTES, SENDO O ULTIMO O DIGITO
               05  BVVEMA-I-CT-CLI           PIC 9(013).
               05  BVVEMA-I-PERC-GAR         PIC 9(005).
               05  BVVEMA-I-DTINI-VALIDD     PIC 9(008).
               05  BVVEMA-I-DTFIM-VALIDD     PIC 9(008).
               05  BVVEMA-I-TP-NATU-GAR      PIC X(001).
               05  BVVEMA-I-DT-MOVTO         PIC 9(008).
               05  BVVEMA-I-BANDEIRA         PIC 9(003).
               05  BVVEMA-I-VLR-MAX-RET      PIC 9(015)V99.
               05  FILLER                    PIC X(017).
           03  BVVEMA-SCG0012 REDEFINES BVVEMA-SCG0011.
               05  BVVEMA-A-DADOS-CLI.
                   07 BVVEMA-A-TP-PESSOA-CLI PIC X(001).
                   07 BVVEMA-A-CNPJ-CPF-CLI  PIC 9(014).
                   07 BVVEMA-A-CPF           REDEFINES
                      BVVEMA-A-CNPJ-CPF-CLI.
                      09 BVVEMA-A-ZEROS      PIC 9(003).
                      09 BVVEMA-A-CPF-NUM    PIC 9(011).
               05  BVVEMA-A-TRAVA-RAIZ       PIC X(001).
               05  BVVEMA-A-NUM-IDENTC-SCG   PIC 9(017).
               05  BVVEMA-A-AG-CLI           PIC 9(004).
               05  BVVEMA-A-CT-CLI           PIC 9(013).
               05  BVVEMA-A-PERC-GAR         PIC 9(005).
               05  BVVEMA-A-DTINI-VALIDD     PIC 9(008).
               05  BVVEMA-A-DTFIM-VALIDD     PIC 9(008).
               05  BVVEMA-A-TP-NATU-GAR      PIC X(001).
               05  BVVEMA-A-DT-MOVTO         PIC 9(008).
               05  BVVEMA-A-BANDEIRA         PIC 9(003).
               05  BVVEMA-A-COD-PROP-ESQ     PIC X(003).
               05  BVVEMA-A-VLR-MAX-RET      PIC 9(015)V99.
               05  FILLER                    PIC X(004).
           03  BVVEMA-SCG0013 REDEFINES BVVEMA-SCG0011.
               05  BVVEMA-E-DADOS-CLI.
                   07 BVVEMA-E-TP-PESSOA-CLI PIC X(001).
                   07 BVVEMA-E-CNPJ-CPF-CLI  PIC 9(014).
                   07 BVVEMA-E-CPF           REDEFINES
                      BVVEMA-E-CNPJ-CPF-CLI.
                      09 BVVEMA-E-ZEROS      PIC 9(003).
                      09 BVVEMA-E-CPF-NUM    PIC 9(011).
               05  BVVEMA-E-TRAVA-RAIZ       PIC X(001).
               05  BVVEMA-E-NUM-IDENTC-SCG   PIC 9(017).
               05  BVVEMA-E-TP-BAIXA-DOMCL   PIC X(001).
               05  BVVEMA-E-DT-MOVTO         PIC 9(008).
               05  BVVEMA-E-BANDEIRA         PIC 9(003).
               05  BVVEMA-E-COD-PROPT-ESQM   PIC X(003).
               05  FILLER                    PIC X(059).
      *    BVVEMA-I-CNPJ-CPF-CLI CONTERA O CNPJ OU CPF, SE FOR CPF ESTA
      *    ALINHADO A DIREITA , COM ZEROS A ESQUERDA
