      *****************************************************************
      * NOME DA INC - I#GFCTT6                                        *
      * DESCRICAO   - FLEXIBILIZACAO AGRUPAMENTO - SAIDA   -  GFCT1705*
      *               BATCH DEFERIMENTO RELATO   - ENTRADA -  GFCT1720*
      * TAMANHO     - 200                                             *
      * GFCTT6-LL   - 200                                             *
      * DATA        - 31.05.2006                                      *
      * RESPONSAVEL - VALERIA - PROCWORK                              *
      *****************************************************************
      *
       01  GFCTT6-REGISTRO.
           03  GFCTT6-DADOS-CHAVE.
               05  GFCTT6-CSERVC-TARIF              PIC  9(005).
               05  GFCTT6-CAGPTO-CTA                PIC  9(003).
               05  GFCTT6-HIDTFD-FLEXZ-AGPTO        PIC  X(026).
           03  GFCTT6-DADOS.
               05  GFCTT6-DINIC-FLEXZ-AGPTO         PIC  X(010).
               05  GFCTT6-DFIM-FLEXZ-AGPTO          PIC  X(010).
               05  GFCTT6-CINDCD-AGPTO-TOT          PIC  X(001).
               05  GFCTT6-CDEPDC-ORIGE-FLEXZ        PIC  9(005).
               05  GFCTT6-CDEPDC-PAREC-FLEXZ        PIC  9(005).
               05  GFCTT6-DATA-SOLTC-FLEXZ          PIC  X(010).
               05  GFCTT6-DATA-PAREC-FLEXZ          PIC  X(010).
           03  GFCTT6-DADOS-AGPTO04.
ST2506*        05  GFCTT6-CCGC-CPF04                PIC  X(009).
ST2506*        05  GFCTT6-CFLIAL-CGC04              PIC  X(005).
ST2506         05  GFCTT6-CCGC-CPF04                PIC  X(009).
ST2506         05  GFCTT6-CFLIAL-CGC04              PIC  X(004).
               05  GFCTT6-CCTRL-CPF-CGC04           PIC  X(002).
           03  GFCTT6-DADOS-AGPTO05.
               05  GFCTT6-CEMPR-INC05               PIC  9(005).
               05  GFCTT6-CDEPDC05                  PIC  9(005).
           03  GFCTT6-DADOS-AGPTO09.
               05  GFCTT6-CSGMTO-GSTAO-TARIF09      PIC  9(003).
           03  GFCTT6-DADOS-AGPTO15.
               05  GFCTT6-CEMPR-INC15               PIC  9(005).
               05  GFCTT6-CDEPDC15                  PIC  9(005).
               05  GFCTT6-CPOSTO-SERVC15            PIC  9(005).
           03  GFCTT6-DADOS-AGPTO16.
               05  GFCTT6-CJUNC-DEPDC16             PIC  9(005).
               05  GFCTT6-CCTA-CLI16                PIC  9(007).
           03  GFCTT6-DADOS-AGPTO17.
               05  GFCTT6-CJUNC-DEPDC17             PIC  9(005).
               05  GFCTT6-CDEPDC17                  PIC  9(005).
               05  GFCTT6-CPOSTO-SERVC17            PIC  9(005).
           03  GFCTT6-DADOS-AGPTO18.
               05  GFCTT6-CMUN-IBGE18               PIC  9(007).
           03  GFCTT6-DADOS-AGPTO19.
               05  GFCTT6-CSGL-UF19                 PIC  X(002).
           03  GFCTT6-MENSAGEM                      PIC  X(015).
           03  GFCTT6-DADOS-2.
               05  GFCTT6-CPER-COBR-TARIF           PIC  9(001).
               05  GFCTT6-CINIC-PER-COBR            PIC  9(001).
           03  GFCTT6-DADOS-AGPTO26.
               05  GFCTT6-EMPR-CTBIL26              PIC  9(005).
               05  GFCTT6-GRP-CTBIL26               PIC  9(003).
               05  GFCTT6-SGRP-CTBIL26              PIC  9(003).
ST2506*    03  FILLER                               PIC  X(007).
ST2506     03  FILLER                               PIC  X(008).
