      ******************************************************************
      * NOME BOOK   : CMTFWSNB                                         *
      * DESCRICAO   : LAYOUT TABELAS GFCTB0E7(LEGADO) E GOTFB0C6(NAS)  *
      *                              GFCTB0E3(LEGADO) E GOTFB0C4(NAS)  *
      * DATA        : MAIO / 2012                                      *
      * EMPRESA     : SONDA PROCWORK                                   *
      * AUTOR       : SONDA PROCWORK                                   *
      * GRUPO       : CMTF - CONVIVENCIA E MIGRACAO TARIFAS            *
      * TAMANHO     : 1205 BYTES                                       *
      ******************************************************************
      *   DATA          AUTOR             DESCRICAO / MANUTENCAO       *
      * DD/MM/AA     XXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                              XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
      *
           05 CMTFWSNB-REGISTRO.
              10 CMTFWSNB-TAB-GFCTB0E7.
                 15 CMTFWSNB-GFCTB0E7-CHAVE.
                    20 CMTFWSNB-CSERVC-TARIF           PIC  9(005).
                    20 CMTFWSNB-CAGPTO-CTA             PIC  9(003).
                    20 CMTFWSNB-DINIC-SUSP-COBR        PIC  X(010).
                    20 CMTFWSNB-CSEQ-AGPTO-CTA         PIC  9(009).
                 15 CMTFWSNB-CFUNC-MANUT               PIC  9(009).
                 15 CMTFWSNB-CFUNC-MANUT-INCL          PIC  9(009).
                 15 CMTFWSNB-DFIM-SUSP-COBR            PIC  X(010).
                 15 CMTFWSNB-CINDCD-LIBRC-COBR         PIC  X(001).
                 15 CMTFWSNB-DCOBR-SUSP-LIBRD          PIC  X(010).
                 15 CMTFWSNB-RJUSTF-SUSP-COBR.
                    49 CMTFWSNB-RJUSTF-SUSP-COBR-LEN   PIC  9(004).
                    49 CMTFWSNB-RJUSTF-SUSP-COBR-TXT   PIC  X(255).
                 15 CMTFWSNB-CINDCD-AGPTO-TOT          PIC  X(001).
                 15 CMTFWSNB-HMANUT-REG                PIC  X(026).
                 15 CMTFWSNB-HINCL-REG-SIST            PIC  X(026).
      *-------------------------------------------------------------*
      *    AREA RESERVADA PARA DADOS DAS TABELAS FILHAS (LEGADO)    *
      *-------------------------------------------------------------*
              10 CMTFWSNB-TAB-GFCTB0E3.
                 15 CMTFWSNB-CCGC-CPF                  PIC  9(009).
                 15 CMTFWSNB-CFLIAL-CGC                PIC  9(005).
                 15 CMTFWSNB-CCTRL-CPF-CGC             PIC  9(002).
      *
              10 CMTFWSNB-TAB-GOTFB0C6.
                 15 CMTFWSNB-GOTFB0C6-CHAVE.
                    20 CMTFWSNB-CPSSOA-JURID-CONGL     PIC  9(010).
                    20 CMTFWSNB-CGRP-CONTR-NEGOC       PIC  9(003).
                    20 CMTFWSNB-CPRODT-OPER-DEFLT      PIC  9(008).
                    20 CMTFWSNB-COPER-PRODT-SERVC      PIC  9(005).
                    20 CMTFWSNB-CCONDC-ECONC           PIC  9(005).
                    20 CMTFWSNB-NSUSP-COBR-TARIF       PIC  9(005).
                 15 CMTFWSNB-DINIC-SUSP-COBR           PIC  X(010).
                 15 CMTFWSNB-DFIM-SUSP-COBR            PIC  X(010).
                 15 CMTFWSNB-CINDCD-COBR-PEND          PIC  9(001).
                 15 CMTFWSNB-DCOBR-TARIF-SUSP          PIC  X(010).
                 15 CMTFWSNB-CINDCD-TPO-PER            PIC  9(001).
                 15 CMTFWSNB-DINIC-PER-COBR            PIC  X(010).
                 15 CMTFWSNB-DFIM-PER-COBR             PIC  X(010).
                 15 CMTFWSNB-CUSUAR-INCL               PIC  X(009).
                 15 CMTFWSNB-HINCL-REG                 PIC  X(026).
                 15 CMTFWSNB-CUSUAR-MANUT              PIC  X(009).
                 15 CMTFWSNB-HMANUT-REG                PIC  X(026).
                 15 CMTFWSNB-CUSUAR-EXCL               PIC  X(009).
                 15 CMTFWSNB-HEXCL-REG                 PIC  X(026).
                 15 CMTFWSNB-RJUSTF-SUSP-COBR.
                    49 CMTFWSNB-RJUSTF-SUSP-COBR-LEN   PIC  9(004).
                    49 CMTFWSNB-RJUSTF-SUSP-COBR-TXT   PIC  X(300).
                 15 CMTFWSNB-RJUSTF-ENCRR-SUSP.
                    49 CMTFWSNB-RJUSTF-ENCRR-SUSP-LEN  PIC  9(004).
                    49 CMTFWSNB-RJUSTF-ENCRR-SUSP-TXT  PIC  X(300).
              10 CMTFWSNB-TAB-GOTFB0C4.
                 15 CMTFWSNB-CPSSOA                    PIC  9(010).
      *
