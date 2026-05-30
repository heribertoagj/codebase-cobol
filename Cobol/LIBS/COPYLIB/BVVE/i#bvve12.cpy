      ******************************************************************
      *                                                                *
      *   AREA     : I#BVVE12                                          *
      *   TAMANHO  : 258                                               *
      *   OBJETIVO : ARQUIVO DE TRANSFERENCIAS ONLINE DO DOMICILIO     *
      *              BANCARIO ACATADAS PELA CIELO ( VSAM TDBCIELA )    *
      *                                                                *
      ******************************************************************

       01  TDB-CIELO-A.
           03  TDB-CHAVE-NOVA-A.
               05  TDB-DINCL-CHAVA-A   PIC 9(008).
               05  TDB-CCNPJ-CPF-ESTBL-A
                                       PIC 9(009).
               05  TDB-CFLIAL-CNPJ-ESTBL-A
                                       PIC 9(005).
               05  TDB-CPTO-VDA-ESTBL-A
                                       PIC 9(010).
               05  TDB-CTPO-DOMCL-A    PIC 9(001).
               05  TDB-DINCL-ESTBL-COML-A
                                       PIC 9(008).
               05  FILLER              PIC X(022).
           03  TDB-CCTRL-CNPJ-CPF-A    PIC 9(002).
           03  TDB-CBCO-A              PIC 9(003).
           03  TDB-CAG-BCRIA-A         PIC 9(005).
           03  TDB-CCTA-BCRIA-CLI-A    PIC 9(013).
           03  TDB-CBCO-ORIGE-ESTBL-A  PIC 9(003).
           03  TDB-CAG-ORIGE-ESTBL-A   PIC 9(005).
           03  TDB-CCTA-ORIGE-ESTBL-A  PIC 9(013).
           03  TDB-CDIG-CTA-ORIGE-A    PIC X(001).
           03  TDB-CFUNC-BDSCO-A       PIC 9(009).
           03  TDB-CEMPR-TERC-CONTT-A  PIC 9(004).
           03  TDB-HINCL-A             PIC X(026).
           03  FILLER                  PIC X(111).
