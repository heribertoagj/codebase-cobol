      *================================================================*
      *  I#BVVEN9 - OPERACOES EM LOTE RETORNADAS DA CIP                *
      *  GERADO A PARTIR DO ARRC022RET - COMPLEMENTAR DADOS BRADESCO   *
      *  LRECL 200 BYTES                                               *
      *----------------------------------------------------------------*
      *  BVVEN9-SITUACAO  : 'A' ACEITO  OU 'R' REJEITADO               *
      *  BVVEN9-IND-A-CONS: 'S' OU 'N' (INDICA SE SALDO A CONSTITUIR)  *
      *================================================================*

       01  BVVEN9-RET-ARRC022.
           05 BVVEN9-SITUACAO             PIC X(001).
           05 BVVEN9-ID-OPER-BRAD         PIC X(019).
           05 BVVEN9-ID-OPER-CIP          PIC X(019).
           05 BVVEN9-CNPJ-USUAR           PIC 9(014).
           05 BVVEN9-CPF-USUAR            REDEFINES
              BVVEN9-CNPJ-USUAR.
              07 BVVEN9-NUM-CPF           PIC 9(011).
              07 FILLER                   PIC X(003).
           05 BVVEN9-CNPJ-CREDENC         PIC 9(014).
           05 BVVEN9-ARRANJ-PAGTO         PIC X(003).
           05 BVVEN9-DT-LIQ               PIC X(010).
           05 BVVEN9-VLR-NEGOC            PIC 9(15)V99.
           05 BVVEN9-IND-A-CONS           PIC X(001).
           05 BVVEN9-MOTIVO-REJEI         PIC X(050).
           05 BVVEN9-DADOS-COMPL.
              07 BVVEN9-AGENCIA           PIC 9(005).
              07 BVVEN9-CONTA             PIC 9(013).
              07 BVVEN9-PRODUTO           PIC 9(005).
              07 BVVEN9-CRED              PIC 9(004).
              07 BVVEN9-BAND              PIC 9(003).
           05 BVVEN9-TP-PESSOA            PIC X(001).
           05 FILLER                      PIC X(021).
