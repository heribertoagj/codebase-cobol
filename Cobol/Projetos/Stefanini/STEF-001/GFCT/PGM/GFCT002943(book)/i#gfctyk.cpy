      *****************************************************************
      * NOME DA INC - I#GFCTYK                                        *
      * DESCRICAO   - COBRANCA LIBERACAO SUSPENSAO TARIFAS            *
      * REG TAB LIBERACAO TARIFAS AGRUPMTO - GFCTB0E7 - ROTINA BATCH  *
ST2507* TAMANHO     - 072                                             *
      * DATA        - 24.01.2007                                      *
      * RESPONSAVEL - VALERIA  - PROCWORK                             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTYK-GFCTB0E7.
           05 GFCTYK-CSERVC-TARIF         PIC S9(05)        COMP-3.
           05 GFCTYK-CAGPTO-CTA           PIC S9(03)        COMP-3.
           05 GFCTYK-DINIC-SUSP-COBR      PIC  X(10).
           05 GFCTYK-CSEQ-AGPTO-CTA       PIC S9(09)        COMP-3.
           05 GFCTYK-DFIM-SUSP-COBR       PIC  X(10).
           05 GFCTYK-CINDCD-LIBRC-COBR    PIC  X(01).
           05 GFCTYK-DCOBR-SUSP-LIBRD     PIC  X(10).
           05 GFCTYK-CINDCD-AGPTO-TOT     PIC  X(01).
ST2507*    05 GFCTYK-CCGC-CPF             PIC S9(09)        COMP-3.
ST2507*    05 GFCTYK-CFLIAL-CGC           PIC S9(05)        COMP-3.
ST2507     05 GFCTYK-CCGC-CPF             PIC  X(09).
ST2507     05 GFCTYK-CFLIAL-CGC           PIC  X(04).
           05 GFCTYK-CCTRL-CPF-CGC        PIC S9(02)        COMP-3.
           05 GFCTYK-CEMPR-INC            PIC S9(05)        COMP-3.
           05 GFCTYK-CDEPDC               PIC S9(05)        COMP-3.
           05 GFCTYK-CPOSTO-SERVC         PIC S9(05)        COMP-3.
           05 GFCTYK-CMUN-IBGE            PIC S9(07)        COMP-3.
           05 GFCTYK-CSGL-UF              PIC  X(02).
