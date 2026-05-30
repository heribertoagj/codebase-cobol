      *
      *****************************************************************
      * NOME DA INC - GFCTWBD0                                        *
      * DESCRICAO   - TARIFAS - CONTAS SEM CESTA ADERIDA              *
      * TAMANHO     - 0404                                            *
      * DATA        - MAI/2013                                        *
      * RESPONSAVEL - PAGNOCCA - SONDA IT                             *
      *****************************************************************
      *
       01  GFCTWBD0-REG.
           05  BD0-COD-GERENTE       PIC  9(009).
           05  BD0-TP-CESTA          PIC  X(001).
           05  BD0-FOLHA             PIC  X(001).
           05  BD0-VR-RENTAB         PIC  S9(015)V9(02) COMP-3.
           05  BD0-AG                PIC  9(005).
           05  BD0-CTA               PIC  9(013).
           05  BD0-CPF               PIC  9(009).
           05  BD0-FIL               PIC  9(005).
           05  BD0-CTR               PIC  9(003).
           05  BD0-NOME-CLI          PIC  X(080).
           05  BD0-DT-ABERT-CTA      PIC  X(010).
           05  BD0-F-DDI             PIC  X(004).
           05  BD0-F-DDD             PIC  X(004).
           05  BD0-F-FONE            PIC  9(011).
           05  BD0-F-RAMAL           PIC  X(015).
           05  BD0-VR-INVEST         PIC  S9(015)V9(02) COMP-3.
           05  BD0-PROCON            PIC  X(001).
           05  BD0-CLUB              PIC  9(011).
           05  BD0-TRAG              PIC  X(001).
           05  BD0-FUNC              PIC  X(001).
           05  BD0-INAT              PIC  X(001).
           05  BD0-DATA-GFCT         PIC  X(010).
           05  BD0-TMS-GERACAO       PIC  X(026).
           05  BD0-COMPLEMENTOS.
               10  BD0-CESTA-GFCT    PIC  9(005).
               10  BD0-MSG-RECUSA    PIC  X(080).
               10  FILLER            PIC  X(040).
           05  BD0-RESERVA           PIC  X(040).
      *
      *****************************************************************
      * FIM DA INC - GFCTWBD0                                         *
      *****************************************************************
      *
