      *================================================================*
      *  I#BVVEC3 - RETORNO ARRC022RET PARA GRANDES VOLUMES            *
      *  CLONE DO BOOK BVVEN9 + DADOS GRANDES VOLUMES
      *  LRECL 250 BYTES                                               *
      *----------------------------------------------------------------*
      *  BVVEC3-SITUACAO  : 'A' ACEITO  OU 'R' REJEITADO               *
      *  BVVEC3-IND-A-CONS: 'S' OU 'N' (INDICA SE SALDO A CONSTITUIR)  *
      *================================================================*

       01  BVVEC3-RET-ARRC022.
           05 BVVEC3-SITUACAO             PIC X(001).
           05 BVVEC3-ID-OPER-BRAD         PIC X(019).
           05 BVVEC3-ID-OPER-CIP          PIC X(019).
           05 BVVEC3-CNPJ-USUAR           PIC 9(014).
           05 BVVEC3-CPF-USUAR            REDEFINES
              BVVEC3-CNPJ-USUAR.
              07 BVVEC3-NUM-CPF           PIC 9(011).
              07 FILLER                   PIC X(003).
           05 BVVEC3-CNPJ-CREDENC         PIC 9(014).
           05 BVVEC3-ARRANJ-PAGTO         PIC X(003).
           05 BVVEC3-DT-LIQ               PIC X(010).
           05 BVVEC3-VLR-NEGOC            PIC 9(15)V99.
           05 BVVEC3-IND-A-CONS           PIC X(001).
           05 BVVEC3-MOTIVO-REJEI         PIC X(050).
           05 BVVEC3-DADOS-COMPL.
              07 BVVEC3-AGENCIA           PIC 9(005).
              07 BVVEC3-CONTA             PIC 9(013).
              07 BVVEC3-PRODUTO           PIC 9(005).
              07 BVVEC3-CRED              PIC 9(004).
              07 BVVEC3-BAND              PIC 9(003).
           05 BVVEC3-TP-PESSOA            PIC X(001).
           05 BVVEC3-GRD-VOLUMES.
              07 BVVEC3-PROTOCOLO         PIC X(050).
              07 BVVEC3-VLR-SOLICT        PIC 9(15)V99.
           05 FILLER                      PIC X(069).
