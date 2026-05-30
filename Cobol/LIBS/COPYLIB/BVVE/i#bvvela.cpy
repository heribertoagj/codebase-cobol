      *----------------------------------------------------------------*
      *  I#BVVELA - SOLICITAÇAO AUTORIZACAO DE ENVIO DE AGENDA (OPTIN) *
      *  ARQUIVO ASCG101  - TAMANHO 67                                 *
      *  REQUISICAO DE INCLUSAO DE AUTORIZACAO DE ENVIO DE AGENDA      *
      *----------------------------------------------------------------*

       01  BVVELA-ASCG101.
           03 BVVELA-GRUPO-INCL-AUT-AGDA  PIC X(006).
           03 BVVELA-NUM-CTRL-IF          PIC X(020).
           03 BVVELA-DADOS-CLI.
              05 BVVELA-TP-PESSOA-CLI     PIC X(001).
              05 BVVELA-CNPJ-CPF-CLI      PIC 9(014).
              05 BVVELA-CPF               REDEFINES
                 BVVELA-CNPJ-CPF-CLI.
                 07 BVVELA-ZEROS          PIC 9(003).
                 07 BVVELA-CPF-NUM        PIC 9(011).
           03 BVVELA-CNPJ-BASE-CLI        PIC X(008).
           03 BVVELA-COD-ARRAJ-PGTO       PIC X(003).
           03 BVVELA-IND-DOMCL            PIC X(001).
           03 BVVELA-CNPJ-CREDDR          PIC 9(014).
