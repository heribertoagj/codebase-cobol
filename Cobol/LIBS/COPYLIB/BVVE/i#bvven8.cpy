      *================================================================*
      *  I#BVVEN8 - OPERACOES EM LOTE RETORNADAS DA CIP                *
      *  GERADO A PARTIR DO ARRC022RET                                 *
      *  LRECL 200 BYTES                                               *
      *----------------------------------------------------------------*
      *  BVVEN8-SITUACAO  : 'A' ACEITO  OU 'R' REJEITADO               *
      *  BVVEN8-IND-A-CONS: 'S' OU 'N' (INDICA SE SALDO A CONSTITUIR)  *
      *================================================================*

       01  BVVEN8-RET-ARRC022.
           03 BVVEN8-SITUACAO             PIC X(001).
           03 BVVEN8-ID-OPER-BRAD         PIC X(019).
           03 BVVEN8-ID-OPER-CIP          PIC X(019).
           03 BVVEN8-TP-PSSOA-USUAR       PIC 9(014).
           03 BVVEN8-CNPJ-USUAR           PIC 9(014).
           03 BVVEN8-CPF-USUAR            REDEFINES
              BVVEN8-CNPJ-USUAR.
              05 BVVEN8-NUM-CPF           PIC 9(011).
              05 FILLER                   PIC X(003).
           03 BVVEN8-CNPJ-CREDENC         PIC 9(014).
           03 BVVEN8-ARRANJ-PAGTO         PIC X(003).
           03 BVVEN8-DT-LIQ               PIC X(008).
           03 BVVEN8-VLR-NEGOC            PIC X(020).
           03 BVVEN8-IND-A-CONS           PIC X(001).
           03 BVVEN8-MOTIVO-REJEI         PIC X(050).
           03 BVVEN8-TP-PSSOA             PIC X(001).
           03 BVVEN8-DT-INI-OPER          PIC X(008).
           03 BVVEN8-DT-FIM-OPER          PIC X(008).
           03 BVVEN8-AGENCIA              PIC 9(005).
           03 BVVEN8-CONTA                PIC 9(013).
           03 FILLER                      PIC X(002).
