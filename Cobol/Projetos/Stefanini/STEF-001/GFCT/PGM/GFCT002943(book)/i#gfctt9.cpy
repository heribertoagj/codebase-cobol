      ******************************************************************
      * NOME DA INC - I#GFCTT9                                         *
      * DESCRICAO   - FLEXIBILIZACAO       - GFCT1750, 55, 60, 65 E 70 *
      *               BATCH RELATORIO MENSAL                           *
      * TAMANHO     - 300                                              *
      * GFCTT9-LL   - 300                                              *
      * DATA        - 21.06.2006                                       *
      * RESPONSAVEL - VALERIA - PROCWORK                               *
      ******************************************************************
      *
       01  GFCTT9-REGISTRO.
           03  GFCTT9-DADOS-CHAVE.
               05  GFCTT9-DEPEND-SOL                PIC  9(005).
               05  GFCTT9-DFIM-FLEXZ                PIC  X(010).
               05  GFCTT9-TIPO-FLEX                 PIC  9(002).
      *            01 =  FLEXIBILIZACAO INDIVIDUAL
      *            02 =  FLEXIBILIZACAO VIP INDIVIDUAL
      *            03 =  FLEXIBILIZACAO VIP SEGMENTO
           03  GFCTT9-DADOS-AGENCIA-CTA.
               05  GFCTT9-AGENCIA                   PIC  9(005).
               05  GFCTT9-CONTA                     PIC  9(007).
           03  GFCTT9-DADOS-VIP-AG-CTA.
               05  GFCTT9-AG-VIP                    PIC  9(005).
               05  GFCTT9-CTA-VIP                   PIC  9(007).
           03  GFCTT9-DADOS-VIP-SEGMTO.
               05  GFCTT9-SEGMTO                    PIC  9(005).
           03  GFCTT9-DADOS-AGRUPAMENTO.
               05  GFCTT9-CAGPTO-CTA                PIC  9(003).
ST2506*        05  GFCTT9-CCGC-AGP04                PIC  9(009).
ST2506*        05  GFCTT9-CFLIAL-AGP04              PIC  9(005).
ST2506         05  GFCTT9-CCGC-AGP04                PIC  X(009).
ST2506         05  GFCTT9-CFLIAL-AGP04              PIC  X(004).
               05  GFCTT9-CCTRL-AGP04               PIC  X(002).
               05  GFCTT9-CSGMTO-AGP09              PIC  9(003).
               05  GFCTT9-CEMPR-AGP14               PIC  9(005).
               05  GFCTT9-CDEPDC-AGP14              PIC  9(005).
               05  GFCTT9-CEMPR-AGP15               PIC  9(005).
               05  GFCTT9-CPOSTO-AGP15              PIC  9(005).
               05  GFCTT9-CJUNC-AGP16               PIC  9(005).
               05  GFCTT9-CCTA-AGP16                PIC  9(007).
               05  GFCTT9-CEMPR-AGP17               PIC  9(005).
               05  GFCTT9-CDEPDC-AGP17              PIC  9(005).
               05  GFCTT9-CPOSTO-AGP17              PIC  9(005).
               05  GFCTT9-CMUN-AGP18                PIC  9(007).
               05  GFCTT9-DESCR-MUN-AGP18           PIC  X(030).
               05  GFCTT9-UF-AGP19                  PIC  X(002).
               05  GFCTT9-DESCR-UF-AGP19            PIC  X(030).
           03  GFCTT9-DADOS-FLEX.
               05  GFCTT9-CSERVC-TARIF              PIC  9(005).
               05  GFCTT9-QFRANQ-FLEXZ              PIC  9(003).
               05  GFCTT9-VNEGOC-FLEXZ              PIC  9(009)V99.
               05  GFCTT9-PDESC-FLEXZ               PIC  9(003)V9(004).
               05  GFCTT9-CTPO-OPER-MOTVO           PIC  9(003).
               05  GFCTT9-CMOTVO-JUSTF              PIC  9(002).
               05  GFCTT9-DINIC-FLEXZ               PIC  X(010).
           03  GFCTT9-DADOS-2.
               05  GFCTT9-CINIC-PER-COBR            PIC  9(1).
           03  GFCTT9-DADOS-AGRUP-RAZAO.
               05  GFCTT9-GRP-CTBIL-AGP26           PIC  9(003).
               05  GFCTT9-SGRP-CTBIL-AGP26          PIC  9(003).
           03  FILLER                               PIC  X(062).
