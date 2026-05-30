      ******************************************************************
      * NOME BOOK   : CMTFWS6B                                         *
      * DESCRICAO   : LAYOUT TABELAS GFCTB0B2(LEGADO) E GOTFB01R(NAS)  *
      * DATA        : MAIO / 2012                                      *
      * EMPRESA     : SONDA PROCWORK                                   *
      * AUTOR       : SONDA PROCWORK                                   *
      * GRUPO       : CMTF - CONVIVENCIA E MIGRACAO TARIFAS            *
      * TAMANHO     : 2011 BYTES (ERA 1999 BYTES)                      *
      ******************************************************************
      *   DATA          AUTOR             DESCRICAO / MANUTENCAO       *
      * DD/MM/AA     XXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                              XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
      *
           05 CMTFWS6B-REGISTRO.
              10 CMTFWS6B-TAB-GFCTB0B2.
                 15 CMTFWS6B-GFCTB0B2-CHAVE.
                    20 CMTFWS6B-CAGPTO-CTA             PIC  9(003).
                    20 CMTFWS6B-CSERVC-TARIF           PIC  9(005).
                    20 CMTFWS6B-HIDTFD-FLEXZ-AGPTO     PIC  X(026).
                 15 CMTFWS6B-CFUNC-SOLTC-FLEXZ         PIC  9(009).
                 15 CMTFWS6B-CFUNC-PAREC-FLEXZ         PIC  9(009).
                 15 CMTFWS6B-CTPO-OPER-MOTVO           PIC  9(003).
                 15 CMTFWS6B-CMOTVO-JUSTF              PIC  9(002).
                 15 CMTFWS6B-CEMPR-ORIGE-FLEXZ         PIC  9(005).
                 15 CMTFWS6B-CDEPDC-ORIGE-FLEXZ        PIC  9(005).
                 15 CMTFWS6B-CEMPR-PAREC-FLEXZ         PIC  9(005).
                 15 CMTFWS6B-CDEPDC-PAREC-FLEXZ        PIC  9(005).
                 15 CMTFWS6B-DINIC-FLEXZ-AGPTO         PIC  X(010).
                 15 CMTFWS6B-DFIM-FLEXZ-AGPTO          PIC  X(010).
                 15 CMTFWS6B-QFRANQ-FLEXZ-AGPTO        PIC  9(003).
                 15 CMTFWS6B-CPER-FRANQ-AGPTO          PIC  9(001).
                 15 CMTFWS6B-VNEGOC-FLEXZ-AGPTO        PIC  9(009)V9(2).
                 15 CMTFWS6B-PDESC-FLEXZ-AGPTO         PIC  9(003)V9(4).
                 15 CMTFWS6B-DDIA-AGNDA                PIC  9(002).
                 15 CMTFWS6B-QDIA-FLEXZ-AGPTO          PIC  9(002).
                 15 CMTFWS6B-HSOLTC-FLEXZ              PIC  X(026).
                 15 CMTFWS6B-RJUSTF-SOLTC-FLEXZ.
                    49 CMTFWS6B-RJUSTF-SOLTC-FLEX-LEN  PIC  9(004).
                    49 CMTFWS6B-RJUSTF-SOLTC-FLEX-TXT  PIC  X(255).
                 15 CMTFWS6B-HPAREC-FLEXZ              PIC  X(026).
                 15 CMTFWS6B-RJUSTF-PAREC-FLEXZ.
                    49 CMTFWS6B-RJUSTF-PAREC-FLEX-LEN  PIC  9(004).
                    49 CMTFWS6B-RJUSTF-PAREC-FLEX-TXT  PIC  X(255).
                 15 CMTFWS6B-CSIT-SOLTC-FLEXZ          PIC  9(002).
                 15 CMTFWS6B-CINDCD-AGPTO-TOT          PIC  X(001).
                 15 CMTFWS6B-CINIC-PER-FRANQ           PIC  9(001).
                 15 CMTFWS6B-CFUNC-EXCL-SIST           PIC  9(009).
                 15 CMTFWS6B-CPER-COBR-TARIF           PIC  9(001).
                 15 CMTFWS6B-CINIC-PER-COBR            PIC  9(001).
                 15 CMTFWS6B-CCJTO-SERVC               PIC  9(003).
      *-------------------------------------------------------------*
      *    AREA RESERVADA PARA DADOS DAS TABELAS FILHAS (LEGADO)    *
      *-------------------------------------------------------------*
              10 CMTFWS6B-TAB-GFCT-FILHA.
                 15 FILLER                             PIC  X(030).
      *
              10 CMTFWS6B-TAB-GOTFB01R.
                 15 CMTFWS6B-GOTFB01R-CHAVE.
                    20 CMTFWS6B-CPSSOA-JURID-CONGL     PIC  9(010).
                    20 CMTFWS6B-CGRP-CONTR-NEGOC       PIC  9(003).
                    20 CMTFWS6B-CPRODT-OPER-DEFLT      PIC  9(008).
                    20 CMTFWS6B-COPER-PRODT-SERVC      PIC  9(005).
                    20 CMTFWS6B-CCONDC-ECONC           PIC  9(005).
                    20 CMTFWS6B-NFLEXZ-TARIF           PIC  9(005).
                 15 CMTFWS6B-CREGRA-ALCAD              PIC  9(008).
                 15 CMTFWS6B-NSEQ-SOLTC-SERVC          PIC  9(013).
                 15 CMTFWS6B-CPERDC                    PIC  9(002).
                 15 CMTFWS6B-CMOTVO-CONDC-APLIC        PIC  9(002).
                 15 CMTFWS6B-CDIA-SMNAL-FRANQ          PIC  9(001).
                 15 CMTFWS6B-CSIT-OPER-REALZ           PIC  9(003).
                 15 CMTFWS6B-DINIC-FLEXZ-TARIF         PIC  X(010).
                 15 CMTFWS6B-DFIM-FLEXZ-TARIF          PIC  X(010).
                 15 CMTFWS6B-PDESC-FLEXZ-TARIF         PIC  9(003)V9(4).
                 15 CMTFWS6B-QFRANQ-FLEXZ-TARIF        PIC  9(007).
                 15 CMTFWS6B-VCOBR-FLEXZ-TARIF         PIC  9(013)V9(2).
JAN13************15 CMTFWS6B-QDIA-RTCAO-NEGOC          PIC  9(002).
JAN13            15 CMTFWS6B-QDIA-RTCAO-NEGOC          PIC  9(003).
                 15 CMTFWS6B-DDIA-AGNDA-COBR           PIC  9(002).
                 15 CMTFWS6B-CINDCD-ANTEC-COBR         PIC  9(001).
JAN13************15 CMTFWS6B-PRENTB-RENOV-FLEXZ        PIC  9(003)V9(4).
JAN13            15 CMTFWS6B-VRENTB-RENOV-FLEXZ        PIC  9(013)V9(2).
                 15 CMTFWS6B-QDIA-ANTCD-MSGEM          PIC  9(002).
JAN13************15 CMTFWS6B-QDEP-EFETU-MES            PIC  9(005).
JAN13            15 CMTFWS6B-QDEP-EFETU-MES            PIC  9(007).
JAN13************15 CMTFWS6B-CINDCD-FNALD-FLEXZ        PIC  9(001).
                 15 CMTFWS6B-CINDCD-INCL-FLEXZ         PIC  9(001).
                 15 CMTFWS6B-CUSUAR-VALDC              PIC  X(009).
                 15 CMTFWS6B-HVALDC-REG                PIC  X(026).
                 15 CMTFWS6B-CINDCD-DEFER-FLEXZ        PIC  9(001).
                 15 CMTFWS6B-CUSUAR-INCL               PIC  X(009).
                 15 CMTFWS6B-HINCL-REG                 PIC  X(026).
                 15 CMTFWS6B-CUSUAR-MANUT              PIC  X(009).
                 15 CMTFWS6B-HMANUT-REG                PIC  X(026).
                 15 CMTFWS6B-CUSUAR-EXCL               PIC  X(009).
                 15 CMTFWS6B-HEXCL-REG                 PIC  X(026).
                 15 CMTFWS6B-RJUSTF-FLEXZ-TARIF.
                    49 CMTFWS6B-RJUSTF-FLEXZ-TARF-LEN  PIC  9(004).
                    49 CMTFWS6B-RJUSTF-FLEXZ-TARF-TXT  PIC  X(300).
                 15 CMTFWS6B-RJUSTF-EXCL-FLEXZ.
                    49 CMTFWS6B-RJUSTF-EXCL-FLEXZ-LEN  PIC  9(004).
                    49 CMTFWS6B-RJUSTF-EXCL-FLEXZ-TXT  PIC  X(300).
                 15 CMTFWS6B-RJUSTF-DEFER-FLEXZ.
                    49 CMTFWS6B-RJUSTF-DEFER-FLEX-LEN  PIC  9(004).
                    49 CMTFWS6B-RJUSTF-DEFER-FLEX-TXT  PIC  X(300).
JAN13            15 CMTFWS6B-CMOTVO-EXCL-DESC          PIC  9(002).
      *-------------------------------------------------------------*
      *      AREA RESERVADA PARA DADOS DAS TABELAS FILHAS (NAS)     *
      *-------------------------------------------------------------*
              10 CMTFWS6B-TAB-GOTF-FILHA.
                 15 FILLER                             PIC  X(070).
      *
