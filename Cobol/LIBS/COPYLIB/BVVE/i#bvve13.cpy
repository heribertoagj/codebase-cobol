      ******************************************************************
      *                                                                *
      *   AREA     : I#BVVE13                                          *
      *   TAMANHO  : 263                                               *
      *   OBJETIVO : ARQUIVO DE TRANSFERENCIAS ONLINE DO DOMICILIO     *
      *              BANCARIO ACATADAS PELA CIELO ( VSAM TDBCIELB )    *
      *                                                                *
      ******************************************************************

       01  TDB-CIELO-B.
           03  TDB-CHAVE-NOVA-B.
               05  TDB-CAG-BCRIA-CHAVE-B
                                       PIC 9(005).
               05  TDB-DINCL-CHAVE-B   PIC 9(008).
               05  TDB-CCNPJ-CPF-ESTBL-B
                                       PIC 9(009).
               05  TDB-CFLIAL-CNPJ-ESTBL-B
                                       PIC 9(005).
               05  TDB-CPTO-VDA-ESTBL-B
                                       PIC 9(010).
               05  TDB-CTPO-DOMCL-B    PIC 9(001).
               05  TDB-DINCL-ESTBL-COML-B
                                       PIC 9(008).
               05  FILLER              PIC X(022).
           03  TDB-CCTRL-CNPJ-CPF-B    PIC 9(002).
           03  TDB-CBCO-B              PIC 9(003).
           03  TDB-CAG-BCRIA-B         PIC 9(005).
           03  TDB-CCTA-BCRIA-CLI-B    PIC 9(013).
           03  TDB-CBCO-ORIGE-ESTBL-B  PIC 9(003).
           03  TDB-CAG-ORIGE-ESTBL-B   PIC 9(005).
           03  TDB-CCTA-ORIGE-ESTBL-B  PIC 9(013).
           03  TDB-CDIG-CTA-ORIGE-B    PIC X(001).
           03  TDB-CFUNC-BDSCO-B       PIC 9(009).
           03  TDB-CEMPR-TERC-CONTT-B  PIC 9(004).
           03  TDB-HINCL-B             PIC X(026).
           03  FILLER                  PIC X(111).
