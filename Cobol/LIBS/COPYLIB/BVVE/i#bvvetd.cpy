      ******************************************************************
      *                                                                *
      *   AREA     : I#BVVETD                                          *
      *   TAMANHO  : 250                                               *
      *   OBJETIVO : ARQUIVO DE TRANSFERENCIAS ONLINE DO DOMICILIO     *
      *              BANCARIO ACATADAS PELA CIELO                      *
      *                                                                *
      ******************************************************************
       01  TDB-CIELO.
           03  TDB-CIELO-CH.
               05  TDB-CCNPJ-CPF-ESTBL       PIC 9(09).
               05  TDB-CFLIAL-CNPJ-ESTBL     PIC 9(05).
               05  TDB-CPTO-VDA-ESTBL        PIC 9(10).
               05  TDB-CTPO-DOMCL            PIC 9(01).
      *            0 - VISA / VISA VALE
      *            1 - VISA
      *            2 - VISA VALE
      *            5 - MASTERCARD
               05  TDB-DINCL-ESTBL-COML      PIC 9(08).
               05  FILLER                    PIC X(22).
           03  TDB-CCTRL-CNPJ-CPF            PIC 9(02).
           03  TDB-CBCO                      PIC 9(03).
           03  TDB-CAG-BCRIA                 PIC 9(05).
           03  TDB-CCTA-BCRIA-CLI            PIC 9(13).
           03  TDB-CBCO-ORIGE-ESTBL          PIC 9(03).
           03  TDB-CAG-ORIGE-ESTBL           PIC 9(05).
           03  TDB-CCTA-ORIGE-ESTBL          PIC 9(13).
           03  TDB-CDIG-CTA-ORIGE            PIC X(01).
           03  TDB-CFUNC-BDSCO               PIC 9(09).
           03  TDB-CEMPR-TERC-CONTT          PIC 9(04).
           03  TDB-HINCL                     PIC X(26).
           03  FILLER                        PIC X(111).
