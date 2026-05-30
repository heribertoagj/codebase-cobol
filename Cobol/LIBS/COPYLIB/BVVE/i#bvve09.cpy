      ******************************************************************
      *                                                                *
      *                    BOOK DA TABELA BVVEB026                     *
      *                 PARA PROCESSAMENTO SEQUENCIAL                  *
      *                                                                *
      * LRECL=150                                                      *
      ******************************************************************

       01  B026-BVVEB026.
           05 B026-CCNPJ-CPF-ESTBL     PIC  9(09)  COMP-3.
           05 B026-CFLIAL-CNPJ-ESTBL   PIC  9(05)  COMP-3.
           05 B026-CPTO-VDA-ESTBL      PIC  9(09)  COMP-3.
           05 B026-CTPO-OPER-FINCR     PIC  X(01).
           05 B026-CIDTFD-BANDE-ESTBL  PIC  X(02).
           05 B026-DINCL-ESTBL-COML    PIC  X(10).
           05 B026-CCTRL-CNPJ-CPF      PIC  9(02)  COMP-3.
           05 B026-CBCO                PIC  9(03)  COMP-3.
           05 B026-CAG-BCRIA           PIC  9(05)  COMP-3.
           05 B026-CCTA-BCRIA-CLI      PIC  9(13)  COMP-3.
           05 B026-CBCO-ORIGE-ESTBL    PIC  9(03)  COMP-3.
           05 B026-CBCO-ORIGE-ESTBL-NULL
                                       PIC  X(01).
           05 B026-CAG-ORIGE-ESTBL     PIC  9(05)  COMP-3.
           05 B026-CAG-ORIGE-ESTBL-NULL
                                       PIC  X(01).
           05 B026-CCTA-ORIGE-ESTBL    PIC  9(13)  COMP-3.
           05 B026-CCTA-ORIGE-ESTBL-NULL
                                       PIC  X(01).
           05 B026-CDIG-CTA-ORIGE      PIC  X(01).
           05 B026-CDIG-CTA-ORIGE-NULL PIC  X(01).
           05 B026-CEMPR-INC           PIC  9(05)  COMP-3.
           05 B026-CDEPDC              PIC  9(05)  COMP-3.
           05 B026-CFUNC-BDSCO         PIC  9(09)  COMP-3.
           05 B026-CSIT-SOLTC-PCERO    PIC  X(01).
           05 B026-DRETOR-SOLTC-PCERO  PIC  X(10).
           05 B026-DRETOR-SOLTC-PCERO-NULL
                                       PIC  X(01).
           05 B026-CTRAVA-DOMCL-ESTBL  PIC  X(01).
           05 B026-CTRAVA-DOMCL-ESTBL-NULL
                                       PIC  X(01).
           05 B026-CPTO-VDA-CTLZA      PIC  9(09)  COMP-3.
           05 B026-CPTO-VDA-CTLZA-NULL
                                       PIC  X(01).
           05 B026-HENVIO-INFO-PCERO   PIC  X(26).
           05 B026-HENVIO-INFO-PCERO-NULL
                                       PIC  X(01).
           05 B026-CEMPR-TERC-CONTT    PIC  9(04)  COMP-3.
           05 B026-CEMPR-TERC-CONTT-NULL
                                       PIC  X(01).
           05 FILLER                   PIC  X(31).
