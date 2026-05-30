      ******************************************************************
      * NOME BOOK  : LAYOUT DAS TABELAS REFERENTES A UORG              *
      *              (GFCTB049+GFCTB050+GFCTB0B2+GFCTB0B3)             *
      * DATA       : MAR / 2017                                        *
      * EMPRESA    : CAPGEMINI                                         *
      * AUTOR      : VERA GUIMARAES                                    *
      * TAMANHO    : 1000 BYTES                                        *
      ******************************************************************
       05  CMTFWCLC-HEADER.
           10 CMTFWCLC-COD-LAYOUT            PIC X(08) VALUE 'CMTFWCLC'.
           10 CMTFWCLC-TAM-LAYOUT            PIC 9(05) VALUE 01000.
      *
       05  REG-GFCTB049.
           10 CMTFWCLC-49-CSERVC-TARIF        PIC 9(05).
           10 CMTFWCLC-49-CAGPTO-CTA          PIC 9(03).
           10 CMTFWCLC-49-HIDTFD-FLEXZ-AGPTO  PIC X(26).
           10 CMTFWCLC-49-DINIC-FLEXZ-AGPTO   PIC X(10).
           10 CMTFWCLC-49-DFIM-FLEXZ-AGPTO    PIC X(10).
           10 CMTFWCLC-49-QFRANQ-FLEXZ-AGPTO  PIC 9(03).
           10 CMTFWCLC-49-CPER-FRANQ-AGPTO    PIC 9(01).
           10 CMTFWCLC-49-VNEGOC-FLEXZ-AGPTO  PIC S9(09)V99.
           10 CMTFWCLC-49-PDESC-FLEXZ-AGPTO   PIC S9(03)V9(4).
           10 CMTFWCLC-49-QDIA-FLEXZ-AGPTO    PIC 9(02).
           10 CMTFWCLC-49-CINDCD-AGPTO-TOT    PIC X(01).
           10 CMTFWCLC-49-DDIA-AGNDA          PIC 9(02).
           10 CMTFWCLC-49-CINIC-PER-FRANQ     PIC 9(01).
           10 CMTFWCLC-49-CTPO-OPER-MOTVO     PIC 9(03).
           10 CMTFWCLC-49-CMOTVO-JUSTF        PIC 9(02).
           10 CMTFWCLC-49-CEMPR-INC           PIC 9(05).
           10 CMTFWCLC-49-CDEPDC-ENCRR-VGCIA  PIC 9(05).
           10 CMTFWCLC-49-CFUNC-ENCRR-VGCIA   PIC 9(09).
           10 CMTFWCLC-49-HVGCIA-REGRA        PIC X(26).
           10 CMTFWCLC-49-CPER-COBR-TARIF     PIC 9(01).
           10 CMTFWCLC-49-CINIC-PER-COBR      PIC 9(01).
      *
       05  REG-GFCTB050.
           10 CMTFWCLC-50-CSERVC-TARIF        PIC 9(05).
           10 CMTFWCLC-50-CAGPTO-CTA          PIC 9(03).
           10 CMTFWCLC-50-HIDTFD-FLEXZ-AGPTO  PIC X(26).
           10 CMTFWCLC-50-CCGC-CPF            PIC 9(09).
           10 CMTFWCLC-50-CFLIAL-CGC          PIC 9(05).
           10 CMTFWCLC-50-CCTRL-CPF-CGC       PIC 9(02).
      *
       05  REG-CMTFWB2C.
           10 CMTFWCLC-B2-CAGPTO-CTA          PIC 9(03).
           10 CMTFWCLC-B2-CSERVC-TARIF        PIC 9(05).
           10 CMTFWCLC-B2-HIDTFD-FLEXZ-AGPTO  PIC X(26).
           10 CMTFWCLC-B2-CFUNC-SOLTC-FLEXZ   PIC 9(09).
           10 CMTFWCLC-B2-CFUNC-PAREC-FLEXZ   PIC 9(09).
           10 CMTFWCLC-B2-CTPO-OPER-MOTVO     PIC 9(03).
           10 CMTFWCLC-B2-CMOTVO-JUSTF        PIC 9(02).
           10 CMTFWCLC-B2-CEMPR-ORIGE-FLEXZ   PIC 9(05).
           10 CMTFWCLC-B2-CDEPDC-ORIGE-FLEXZ  PIC 9(05).
           10 CMTFWCLC-B2-CEMPR-PAREC-FLEXZ   PIC 9(05).
           10 CMTFWCLC-B2-CDEPDC-PAREC-FLEXZ  PIC 9(05).
           10 CMTFWCLC-B2-DINIC-FLEXZ-AGPTO   PIC X(10).
           10 CMTFWCLC-B2-DFIM-FLEXZ-AGPTO    PIC X(10).
           10 CMTFWCLC-B2-QFRANQ-FLEXZ-AGPTO  PIC 9(03).
           10 CMTFWCLC-B2-CPER-FRANQ-AGPTO    PIC 9(01).
           10 CMTFWCLC-B2-VNEGOC-FLEXZ-AGPTO  PIC S9(09)V99.
           10 CMTFWCLC-B2-PDESC-FLEXZ-AGPTO   PIC S9(03)V9(4).
           10 CMTFWCLC-B2-DDIA-AGNDA          PIC 9(02).
           10 CMTFWCLC-B2-QDIA-FLEXZ-AGPTO    PIC 9(02).
           10 CMTFWCLC-B2-HSOLTC-FLEXZ        PIC X(26).
           10 CMTFWCLC-B2-RJUSTF-SOLTC-FLEXZ.
              49 CMTFWCLC-B2-SOLTC-LEN
                                              PIC 9(04).
              49 CMTFWCLC-B2-SOLTC-TEXT
                                              PIC X(255).
           10 CMTFWCLC-B2-HPAREC-FLEXZ        PIC X(26).
           10 CMTFWCLC-B2-RJUSTF-PAREC-FLEXZ.
              49 CMTFWCLC-B2-PAREC-LEN
                                              PIC 9(04).
              49 CMTFWCLC-B2-PAREC-TEXT
                                              PIC X(255).
           10 CMTFWCLC-B2-CSIT-SOLTC-FLEXZ    PIC 9(02).
           10 CMTFWCLC-B2-CINDCD-AGPTO-TOT    PIC X(01).
           10 CMTFWCLC-B2-CINIC-PER-FRANQ     PIC 9(01).
           10 CMTFWCLC-B2-CFUNC-EXCL-SIST     PIC 9(09).
           10 CMTFWCLC-B2-CPER-COBR-TARIF     PIC 9(01).
           10 CMTFWCLC-B2-CINIC-PER-COBR      PIC 9(01).
           10 CMTFWCLC-B2-CCJTO-SERVC         PIC 9(03).
      *
       05  REG-CMTFWB3C.
           10 CMTFWCLC-B3-CAGPTO-CTA          PIC 9(03).
           10 CMTFWCLC-B3-CSERVC-TARIF        PIC 9(05).
           10 CMTFWCLC-B3-HIDTFD-FLEXZ-AGPTO  PIC X(26).
           10 CMTFWCLC-B3-CCGC-CPF            PIC 9(09).
           10 CMTFWCLC-B3-CFLIAL-CGC          PIC 9(05).
           10 CMTFWCLC-B3-CCTRL-CPF-CGC       PIC 9(02).
      *
       05 CMTFWCLC-DEFERIDA-FUTURA            PIC X(01).
      *
       05 CMTFWCLC-SAIDA.
           10 CMTFWCLC-SQLCODE                PIC S9(09)      COMP-3.
           10 FILLER                          PIC  X(36).
      *
