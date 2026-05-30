      ******************************************************************
      * NOME BOOK   : CMTFWSND                                         *
      * DESCRICAO   : LAYOUT TABELAS GOTFB0C6(NAS) E GOTFB0C4(NAS)     *
      * DATA        : MAIO / 2012                                      *
      * EMPRESA     : SONDA PROCWORK                                   *
      * AUTOR       : SONDA PROCWORK                                   *
      * GRUPO       : CMTF - CONVIVENCIA E MIGRACAO TARIFAS            *
      * TAMANHO     : 0789 BYTES                                       *
      ******************************************************************
      *   DATA          AUTOR             DESCRICAO / MANUTENCAO       *
      * DD/MM/AA     XXXXXXXXXXX     XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                              XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      ******************************************************************
      *
           05 CMTFWSND-REGISTRO.
              10 CMTFWSND-TAB-GOTFB0C6.
                 15 CMTFWSND-GOTFB0C6-CHAVE.
                    20 CMTFWSND-CPSSOA-JURID-CONGL PIC S9(010) COMP-3.
                    20 CMTFWSND-CGRP-CONTR-NEGOC   PIC S9(003) COMP-3.
                    20 CMTFWSND-CPRODT-OPER-DEFLT  PIC S9(008) COMP-3.
                    20 CMTFWSND-COPER-PRODT-SERVC  PIC S9(005) COMP-3.
                    20 CMTFWSND-CCONDC-ECONC       PIC S9(005) COMP-3.
                    20 CMTFWSND-NSUSP-COBR-TARIF   PIC S9(005) COMP-3.
                 15 CMTFWSND-DINIC-SUSP-COBR       PIC  X(010).
                 15 CMTFWSND-DFIM-SUSP-COBR        PIC  X(010).
                 15 CMTFWSND-CINDCD-COBR-PEND      PIC S9(001) COMP-3.
                 15 CMTFWSND-DCOBR-TARIF-SUSP      PIC  X(010).
                 15 CMTFWSND-CINDCD-TPO-PER        PIC S9(001) COMP-3.
                 15 CMTFWSND-DINIC-PER-COBR        PIC  X(010).
                 15 CMTFWSND-DFIM-PER-COBR         PIC  X(010).
                 15 CMTFWSND-CUSUAR-INCL           PIC  X(009).
                 15 CMTFWSND-HINCL-REG             PIC  X(026).
                 15 CMTFWSND-CUSUAR-MANUT          PIC  X(009).
                 15 CMTFWSND-HMANUT-REG            PIC  X(026).
                 15 CMTFWSND-CUSUAR-EXCL           PIC  X(009).
                 15 CMTFWSND-HEXCL-REG             PIC  X(026).
                 15 CMTFWSND-RJUSTF-SUSP-COBR.
                    49 CMTFWSND-RJUSTF-SUSP-COBR-LEN   PIC S9(004) COMP.
                    49 CMTFWSND-RJUSTF-SUSP-COBR-TXT   PIC  X(300).
                 15 CMTFWSND-RJUSTF-ENCRR-SUSP.
                    49 CMTFWSND-RJUSTF-ENCRR-SUSP-LEN  PIC S9(004) COMP.
                    49 CMTFWSND-RJUSTF-ENCRR-SUSP-TXT  PIC  X(300).
              10 CMTFWSND-TAB-GOTFB0C4.
                 15 CMTFWSND-CPSSOA                PIC S9(010) COMP-3.
