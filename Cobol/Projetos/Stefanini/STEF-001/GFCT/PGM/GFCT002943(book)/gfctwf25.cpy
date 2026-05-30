      *                                                                *
      *----------------------------------------------------------------*
      *  AREA IMAGEM DA TABELA GFCTB0P0 - TPOSIC_SDO_APLIC + CNPJ/CPF  *
      *  BOOK GFCTWF25                                                 *
ST2507*  LRECL 0103                                                    *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
           10 WF25-CBCO                 PIC S9(03)      COMP-3.
           10 WF25-CAG-BCRIA            PIC S9(05)      COMP-3.
           10 WF25-CCTA-CLI             PIC S9(07)      COMP-3.
           10 WF25-DANO-MES-SDO         PIC S9(06)      COMP-3.
           10 WF25-CSERVC-TARIF         PIC S9(05)      COMP-3.
           10 WF25-VSDO-MED-FUNDO       PIC S9(15)V9(2) COMP-3.
           10 WF25-VSDO-MED-POUP        PIC S9(15)V9(2) COMP-3.
           10 WF25-VSDO-MED-CERTF       PIC S9(15)V9(2) COMP-3.
           10 WF25-VSDO-MED-PREVD       PIC S9(15)V9(2) COMP-3.
           10 WF25-VSDO-APLIC-ACOES     PIC S9(15)V9(2) COMP-3.
           10 WF25-VSDO-APLIC-TSORO     PIC S9(15)V9(2) COMP-3.
           10 WF25-VSDO-APLIC-DBTRS     PIC S9(15)V9(2) COMP-3.
           10 WF25-VSDO-APLIC-DERIV     PIC S9(15)V9(2) COMP-3.
ST2507*    10 WF25-CNPJ-CPF             PIC S9(09)      COMP-3.
ST2507*    10 WF25-FILIAL               PIC S9(04)      COMP-3.
ST2507     10 WF25-CNPJ-CPF             PIC  X(09).
ST2507     10 WF25-FILIAL               PIC  X(04).
           10 WF25-DIGITO-FJ            PIC S9(02)      COMP-3.
