      *****************************************************************
      * NOME DA INC - I#GFCTT3                                        *
      * DESCRICAO   - FLEXIBILIZACAO AGRUPAMENTO -            GFCT1700*
      *               BATCH DEFERIMENTO SAIDA                         *
      * TAMANHO     - 400                                             *
      * GFCTT3-LL   - 400                                             *
      * DATA        - 31.05.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTT3-REGISTRO.
           03  GFCTT3-DADOS-CHAVE.
               05  GFCTT3-CSERVC-TARIF              PIC  9(005).
               05  GFCTT3-CAGPTO-CTA                PIC  9(003).
               05  GFCTT3-HIDTFD-FLEXZ-AGPTO        PIC  X(026).
           03  GFCTT3-DADOS.
               05  GFCTT3-DINIC-FLEXZ-AGPTO         PIC  X(010).
               05  GFCTT3-DFIM-FLEXZ-AGPTO          PIC  X(010).
               05  GFCTT3-CINDCD-AGPTO-TOT          PIC  X(001).
               05  GFCTT3-CDEPDC-ORIGE-FLEXZ        PIC  9(005).
               05  GFCTT3-CDEPDC-PAREC-FLEXZ        PIC  9(005).
               05  GFCTT3-DATA-SOLTC-FLEXZ          PIC  X(010).
               05  GFCTT3-DATA-PAREC-FLEXZ          PIC  X(010).
           03  GFCTT3-DADOS1.
               05  GFCTT3-QFRANQ-FLEXZ-AGPTO        PIC  9(003).
               05  GFCTT3-CPER-FRANQ-AGPTO          PIC  9(001).
               05  GFCTT3-VNEGOC-FLEXZ-AGPTO        PIC  9(009)V99.
               05  GFCTT3-PDESC-FLEXZ-AGPTO         PIC  9(003)V9(004).
               05  GFCTT3-QDIA-FLEXZ-AGPTO          PIC  9(002).
               05  GFCTT3-DDIA-AGNDA                PIC  9(002).
               05  GFCTT3-CINIC-PER-FRANQ           PIC  9(001).
               05  GFCTT3-CTPO-OPER-MOTVO           PIC  9(003).
               05  GFCTT3-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTT3-CDEPDC-ENCRR-VGCIA        PIC  9(005).
               05  GFCTT3-CFUNC-ENCRR-VGCIA         PIC  9(009).
               05  GFCTT3-HVGCIA-REGRA              PIC  X(026).
           03  GFCTT3-DADOS-AGPTO04.
ST2506*        05  GFCTT3-CCGC-CPF04                PIC  9(009).
ST2506*        05  GFCTT3-CFLIAL-CGC04              PIC  9(005).
ST2506         05  GFCTT3-CCGC-CPF04                PIC  X(009).
ST2506         05  GFCTT3-CFLIAL-CGC04              PIC  X(004).
               05  GFCTT3-CCTRL-CPF-CGC04           PIC  X(002).
           03  GFCTT3-DADOS-AGPTO05.
               05  GFCTT3-CEMPR-INC05               PIC  9(005).
               05  GFCTT3-CDEPDC05                  PIC  9(005).
           03  GFCTT3-DADOS-AGPTO09.
               05  GFCTT3-CSGMTO-GSTAO-TARIF09      PIC  9(003).
           03  GFCTT3-DADOS-AGPTO15.
               05  GFCTT3-CEMPR-INC15               PIC  9(005).
               05  GFCTT3-CDEPDC15                  PIC  9(005).
               05  GFCTT3-CPOSTO-SERVC15            PIC  9(005).
           03  GFCTT3-DADOS-AGPTO16.
               05  GFCTT3-CJUNC-DEPDC16             PIC  9(005).
               05  GFCTT3-CCTA-CLI16                PIC  9(007).
           03  GFCTT3-DADOS-AG16.
               05  GFCTT3-CAG-EMPR16                PIC  9(005).
               05  GFCTT3-CRZ-CTA-EMPR16            PIC  9(005).
               05  GFCTT3-CNRO-CTA-DEB16            PIC  9(013).
               05  GFCTT3-CLCTO-EMPR16              PIC  9(003).
               05  GFCTT3-PPGTO-RTEIO-EMPR16        PIC  9(003)V9(004).
               05  GFCTT3-RSEGDA-LIN-EXTRT16        PIC  X(032).
           03  GFCTT3-DADOS-AGPTO17.
               05  GFCTT3-CEMPR-INC17               PIC  9(005).
               05  GFCTT3-CDEPDC17                  PIC  9(005).
               05  GFCTT3-CPOSTO-SERVC17            PIC  9(005).
           03  GFCTT3-DADOS-AGPTO18.
               05  GFCTT3-CMUN-IBGE18               PIC  9(007).
           03  GFCTT3-DADOS-AGPTO19.
               05  GFCTT3-CSGL-UF19                 PIC  X(002).
           03  GFCTT3-DADOS-2.
               05  GFCTT3-CPER-COBR-TARIF           PIC  9(001).
               05  GFCTT3-CINIC-PER-COBR            PIC  9(001).
           03  GFCTT3-DADOS-AGPTO26.
               05  GFCTT3-EMPR-CTBIL26              PIC  9(005).
               05  GFCTT3-GRP-CTBIL26               PIC  9(003).
               05  GFCTT3-SGRP-CTBIL26              PIC  9(003).
ST2506*    03  FILLER                               PIC  X(085).
ST2506     03  FILLER                               PIC  X(086).
