      ******************************************************************
      * NOME BOOK   : CMTFWS1B                                         *
      * DESCRICAO   : LAYOUT TABELAS GFCTB0B2(LEGADO)  GOTFB0B3(LEGADO)*
      *                              GOTFB01R(NAS)     GOTFB01F(NAS)   *
      * DATA        : MAIO / 2012                                      *
      * EMPRESA     : SONDA PROCWORK                                   *
      * AUTOR       : SONDA PROCWORK                                   *
      * GRUPO       : CMTF - CONVIVENCIA E MIGRACAO TARIFAS            *
      * TAMANHO     : 02011 BYTES (ERA 1999 BYTES)                     *
      ******************************************************************
      *   DATA          AUTOR             DESCRICAO / MANUTENCAO       *
      * DD/MM/AA     XXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                              XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
      *
           05 CMTFWS1B-REGISTRO.
              10 CMTFWS1B-TAB-GFCTB0B2.
                 15 CMTFWS1B-GFCTB0B2-CHAVE.
                    20 CMTFWS1B-CAGPTO-CTA             PIC  9(003).
                    20 CMTFWS1B-CSERVC-TARIF           PIC  9(005).
                    20 CMTFWS1B-HIDTFD-FLEXZ-AGPTO     PIC  X(026).
                 15 CMTFWS1B-CFUNC-SOLTC-FLEXZ         PIC  9(009).
                 15 CMTFWS1B-CFUNC-PAREC-FLEXZ         PIC  9(009).
                 15 CMTFWS1B-CTPO-OPER-MOTVO           PIC  9(003).
                 15 CMTFWS1B-CMOTVO-JUSTF              PIC  9(002).
                 15 CMTFWS1B-CEMPR-ORIGE-FLEXZ         PIC  9(005).
                 15 CMTFWS1B-CDEPDC-ORIGE-FLEXZ        PIC  9(005).
                 15 CMTFWS1B-CEMPR-PAREC-FLEXZ         PIC  9(005).
                 15 CMTFWS1B-CDEPDC-PAREC-FLEXZ        PIC  9(005).
                 15 CMTFWS1B-DINIC-FLEXZ-AGPTO         PIC  X(010).
                 15 CMTFWS1B-DFIM-FLEXZ-AGPTO          PIC  X(010).
                 15 CMTFWS1B-QFRANQ-FLEXZ-AGPTO        PIC  9(003).
                 15 CMTFWS1B-CPER-FRANQ-AGPTO          PIC  9(001).
                 15 CMTFWS1B-VNEGOC-FLEXZ-AGPTO        PIC  9(009)V9(2).
                 15 CMTFWS1B-PDESC-FLEXZ-AGPTO         PIC  9(003)V9(4).
                 15 CMTFWS1B-DDIA-AGNDA                PIC  9(002).
                 15 CMTFWS1B-QDIA-FLEXZ-AGPTO          PIC  9(002).
                 15 CMTFWS1B-HSOLTC-FLEXZ              PIC  X(026).
                 15 CMTFWS1B-RJUSTF-SOLTC-FLEXZ.
                    49 CMTFWS1B-RJUSTF-SOLTC-FLEX-LEN  PIC  9(004).
                    49 CMTFWS1B-RJUSTF-SOLTC-FLEX-TXT  PIC  X(255).
                 15 CMTFWS1B-HPAREC-FLEXZ              PIC  X(026).
                 15 CMTFWS1B-RJUSTF-PAREC-FLEXZ.
                    49 CMTFWS1B-RJUSTF-PAREC-FLEX-LEN  PIC  9(004).
                    49 CMTFWS1B-RJUSTF-PAREC-FLEX-TXT  PIC  X(255).
                 15 CMTFWS1B-CSIT-SOLTC-FLEXZ          PIC  9(002).
                 15 CMTFWS1B-CINDCD-AGPTO-TOT          PIC  X(001).
                 15 CMTFWS1B-CINIC-PER-FRANQ           PIC  9(001).
                 15 CMTFWS1B-CFUNC-EXCL-SIST           PIC  9(009).
                 15 CMTFWS1B-CPER-COBR-TARIF           PIC  9(001).
                 15 CMTFWS1B-CINIC-PER-COBR            PIC  9(001).
                 15 CMTFWS1B-CCJTO-SERVC               PIC  9(003).
              10 CMTFWS1B-TAB-GFCTB0B3.
                 15 CMTFWS1B-CCGC-CPF                  PIC  9(009).
                 15 CMTFWS1B-CFLIAL-CGC                PIC  9(004).
                 15 CMTFWS1B-CCTRL-CPF-CGC             PIC  9(002).
                 15 FILLER                             PIC  X(015).
      *
              10 CMTFWS1B-TAB-GOTFB01R.
                 15 CMTFWS1B-GOTFB01R-CHAVE.
                    20 CMTFWS1B-CPSSOA-JURID-CONGL     PIC  9(010).
                    20 CMTFWS1B-CGRP-CONTR-NEGOC       PIC  9(003).
                    20 CMTFWS1B-CPRODT-OPER-DEFLT      PIC  9(008).
                    20 CMTFWS1B-COPER-PRODT-SERVC      PIC  9(005).
                    20 CMTFWS1B-CCONDC-ECONC           PIC  9(005).
                    20 CMTFWS1B-NFLEXZ-TARIF           PIC  9(005).
                 15 CMTFWS1B-CREGRA-ALCAD              PIC  9(008).
                 15 CMTFWS1B-NSEQ-SOLTC-SERVC          PIC  9(013).
                 15 CMTFWS1B-CPERDC                    PIC  9(002).
                 15 CMTFWS1B-CMOTVO-CONDC-APLIC        PIC  9(002).
                 15 CMTFWS1B-CDIA-SMNAL-FRANQ          PIC  9(001).
                 15 CMTFWS1B-CSIT-OPER-REALZ           PIC  9(003).
                 15 CMTFWS1B-DINIC-FLEXZ-TARIF         PIC  X(010).
                 15 CMTFWS1B-DFIM-FLEXZ-TARIF          PIC  X(010).
                 15 CMTFWS1B-PDESC-FLEXZ-TARIF         PIC  9(003)V9(4).
                 15 CMTFWS1B-QFRANQ-FLEXZ-TARIF        PIC  9(007).
                 15 CMTFWS1B-VCOBR-FLEXZ-TARIF         PIC  9(013)V9(2).
JAN13************15 CMTFWS1B-QDIA-RTCAO-NEGOC          PIC  9(002).
JAN13            15 CMTFWS1B-QDIA-RTCAO-NEGOC          PIC  9(003).
                 15 CMTFWS1B-DDIA-AGNDA-COBR           PIC  9(002).
                 15 CMTFWS1B-CINDCD-ANTEC-COBR         PIC  9(001).
JAN13************15 CMTFWS1B-PRENTB-RENOV-FLEXZ        PIC  9(003)V9(4).
JAN13            15 CMTFWS1B-VRENTB-RENOV-FLEXZ        PIC  9(013)V9(2).
                 15 CMTFWS1B-QDIA-ANTCD-MSGEM          PIC  9(002).
JAN13************15 CMTFWS1B-QDEP-EFETU-MES            PIC  9(005).
JAN13            15 CMTFWS1B-QDEP-EFETU-MES            PIC  9(007).
JAN13************15 CMTFWS1B-CINDCD-FNALD-FLEXZ        PIC  9(001).
                 15 CMTFWS1B-CINDCD-INCL-FLEXZ         PIC  9(001).
                 15 CMTFWS1B-CUSUAR-VALDC              PIC  X(009).
                 15 CMTFWS1B-HVALDC-REG                PIC  X(026).
                 15 CMTFWS1B-CINDCD-DEFER-FLEXZ        PIC  9(001).
                 15 CMTFWS1B-CUSUAR-INCL               PIC  X(009).
                 15 CMTFWS1B-HINCL-REG                 PIC  X(026).
                 15 CMTFWS1B-CUSUAR-MANUT              PIC  X(009).
                 15 CMTFWS1B-HMANUT-REG                PIC  X(026).
                 15 CMTFWS1B-CUSUAR-EXCL               PIC  X(009).
                 15 CMTFWS1B-HEXCL-REG                 PIC  X(026).
                 15 CMTFWS1B-RJUSTF-FLEXZ-TARIF.
                    49 CMTFWS1B-RJUSTF-FLEXZ-TARF-LEN  PIC  9(004).
                    49 CMTFWS1B-RJUSTF-FLEXZ-TARF-TXT  PIC  X(300).
                 15 CMTFWS1B-RJUSTF-EXCL-FLEXZ.
                    49 CMTFWS1B-RJUSTF-EXCL-FLEXZ-LEN  PIC  9(004).
                    49 CMTFWS1B-RJUSTF-EXCL-FLEXZ-TXT  PIC  X(300).
                 15 CMTFWS1B-RJUSTF-DEFER-FLEXZ.
                    49 CMTFWS1B-RJUSTF-DEFER-FLEX-LEN  PIC  9(004).
                    49 CMTFWS1B-RJUSTF-DEFER-FLEX-TXT  PIC  X(300).
JAN13            15 CMTFWS1B-CMOTVO-EXCL-DESC          PIC  9(002).
      *
              10 CMTFWS1B-TAB-GOTFB01F.
                 15 CMTFWS1B-GOTFB01F-CHAVE.
                    20 CMTFWS1B-CPSSOA-JURID-CONGL     PIC  9(010).
                    20 CMTFWS1B-CGRP-CONTR-NEGOC       PIC  9(003).
                    20 CMTFWS1B-CPRODT-OPER-DEFLT      PIC  9(008).
                    20 CMTFWS1B-COPER-PRODT-SERVC      PIC  9(005).
                    20 CMTFWS1B-CCONDC-ECONC           PIC  9(005).
                    20 CMTFWS1B-NFLEXZ-TARIF           PIC  9(005).
                 15 CMTFWS1B-CPSSOA                    PIC  9(010).
                 15 FILLER                             PIC  X(024).
      *
