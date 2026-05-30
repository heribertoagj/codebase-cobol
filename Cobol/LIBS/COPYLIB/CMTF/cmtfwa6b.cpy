      ******************************************************************
      * NOME BOOK   : CMTFWA6B                                         *
      * DESCRICAO   : LAYOUT ARQUIVO CEPTB016(LEGADO) E GOTFB01R(NAS)  *
      *                                               E GOTFB01F(NAS)  *
      *                                               E GOTFB02E(NAS). *
      * DATA        : ABRIL / 2012                                     *
      * EMPRESA     : SONDA PROCWORK                                   *
      * AUTOR       : SONDA PROCWORK                                   *
      * GRUPO       : CMTF  - CONVIVENCIA E MIGRACAO TARIFAS           *
      * TAMANHO     : 01174 BYTES                                      *
      ******************************************************************
      *   DATA          AUTOR             DESCRICAO / MANUTENCAO       *
      * DD/MM/AA     XXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                              XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
           05 CMTFWA6B-REG.
              10 CMTFWA6B-TAB-GOTFB01R.
                 15 CMTFWA6B-CPSSOA-JURID-CONGL  PIC S9(010) COMP-3.
                 15 CMTFWA6B-CGRP-CONTR-NEGOC    PIC S9(003) COMP-3.
                 15 CMTFWA6B-CPRODT-OPER-DEFLT   PIC S9(008) COMP-3.
                 15 CMTFWA6B-COPER-PRODT-SERVC   PIC S9(005) COMP-3.
                 15 CMTFWA6B-CCONDC-ECONC        PIC S9(005) COMP-3.
                 15 CMTFWA6B-NFLEXZ-TARIF        PIC S9(005) COMP-3.
                 15 CMTFWA6B-CREGRA-ALCAD        PIC S9(008) COMP-3.
                 15 CMTFWA6B-NSEQ-SOLTC-SERVC    PIC S9(013) COMP-3.
                 15 CMTFWA6B-CPERDC              PIC S9(002) COMP-3.
                 15 CMTFWA6B-CMOTVO-CONDC-APLIC  PIC S9(002) COMP-3.
                 15 CMTFWA6B-CDIA-SMNAL-FRANQ    PIC S9(001) COMP-3.
                 15 CMTFWA6B-CSIT-OPER-REALZ     PIC S9(003) COMP-3.
                 15 CMTFWA6B-DINIC-FLEXZ-TARIF   PIC  X(010).
                 15 CMTFWA6B-DFIM-FLEXZ-TARIF    PIC  X(010).
                 15 CMTFWA6B-PDESC-FLEXZ-TARIF   PIC S9(03)V9(4) COMP-3.
                 15 CMTFWA6B-QFRANQ-FLEXZ-TARIF  PIC S9(007) COMP-3.
                 15 CMTFWA6B-VCOBR-FLEXZ-TARIF   PIC S9(13)V9(2) COMP-3.
JAN13************15 CMTFWA6B-QDIA-RTCAO-NEGOC    PIC S9(002) COMP-3.
JAN13            15 CMTFWA6B-QDIA-RTCAO-NEGOC    PIC S9(003) COMP-3.
                 15 CMTFWA6B-DDIA-AGNDA-COBR     PIC S9(002) COMP-3.
                 15 CMTFWA6B-CINDCD-ANTEC-COBR   PIC S9(001) COMP-3.
JAN13************15 CMTFWA6B-PRENTB-RENOV-FLEXZ  PIC S9(03)V9(4) COMP-3.
JAN13            15 CMTFWA6B-VRENTB-RENOV-FLEXZ  PIC S9(13)V9(2) COMP-3.
                 15 CMTFWA6B-QDIA-ANTCD-MSGEM    PIC S9(002) COMP-3.
JAN13************15 CMTFWA6B-QDEP-EFETU-MES      PIC S9(005) COMP-3.
JAN13            15 CMTFWA6B-QDEP-EFETU-MES      PIC S9(007) COMP-3.
JAN13************15 CMTFWA6B-CINDCD-FNALD-FLEXZ  PIC S9(001) COMP-3.
                 15 CMTFWA6B-CINDCD-INCL-FLEXZ   PIC S9(001) COMP-3.
                 15 CMTFWA6B-CUSUAR-VALDC        PIC  X(009).
                 15 CMTFWA6B-HVALDC-REG          PIC  X(026).
                 15 CMTFWA6B-CINDCD-DEFER-FLEXZ  PIC S9(001) COMP-3.
                 15 CMTFWA6B-CUSUAR-INCL         PIC  X(009).
                 15 CMTFWA6B-HINCL-REG           PIC  X(026).
                 15 CMTFWA6B-CUSUAR-MANUT        PIC  X(009).
                 15 CMTFWA6B-HMANUT-REG          PIC  X(026).
                 15 CMTFWA6B-HEXCL-REG           PIC  X(026).
                 15 CMTFWA6B-CUSUAR-EXCL         PIC  X(009).
                 15 CMTFWA6B-RJUSTF-FLEXZ-TARIF.
                    49 CMTFWA6B-RJUSTF-FLEXZ-LEN  PIC S9(004) COMP.
                    49 CMTFWA6B-RJUSTF-FLEXZ-TEXT PIC  X(300).
                 15 CMTFWA6B-RJUSTF-EXCL-FLEXZ.
                    49 CMTFWA6B-RJUSTF-EXCL-LEN    PIC S9(004) COMP.
                    49 CMTFWA6B-RJUSTF-EXCL-TEXT   PIC  X(300).
                 15 CMTFWA6B-RJUSTF-DEFER-FLEXZ.
                    49 CMTFWA6B-RJUSTF-DEFER-LEN   PIC S9(004) COMP.
                    49 CMTFWA6B-RJUSTF-DEFER-TEXT  PIC  X(300).
JAN13            15 CMTFWA6B-CMOTVO-EXCL-DESC      PIC S9(002) COMP-3.
      *
              10 CMTFWA6B-TAB-GOTFB01F.
                 15 CMTFWA6B-CPSSOA-JURID-CONGL  PIC S9(010) COMP-3.
                 15 CMTFWA6B-CGRP-CONTR-NEGOC    PIC S9(003) COMP-3.
                 15 CMTFWA6B-CPRODT-OPER-DEFLT   PIC S9(008) COMP-3.
                 15 CMTFWA6B-COPER-PRODT-SERVC   PIC S9(005) COMP-3.
                 15 CMTFWA6B-CCONDC-ECONC        PIC S9(005) COMP-3.
                 15 CMTFWA6B-NFLEXZ-TARIF        PIC S9(005) COMP-3.
                 15 CMTFWA6B-CPSSOA              PIC S9(010) COMP-3.
              10 CMTFWA6B-TAB-GFCTB0B3.
                 15 CMTFWA6B-CCGC-CPF            PIC  9(009).
                 15 CMTFWA6B-CFLIAL-CGC          PIC  9(005).
                 15 CMTFWA6B-CCTRL-CPF-CGC       PIC  9(002).
              10 FILLER                          PIC  X(050).
