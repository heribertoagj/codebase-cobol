      ******************************************************************
      * NOME BOOK: CMTFWZKR                                            *
      * DESCRICAO: ARQUIVO INFORMATIVO DO ESPELHAMENTO -> GFCT X GOTF  *
      *            SUSPENSAO POR CLIENTE.                              *
      * EMPRESA..: CAPGEMINI                                           *
      * AUTOR....: GEORGIA                                             *
      * DATA.....: 25/10/2017                                          *
      * TAMANHO..: 200 BYTES                                           *
      ******************************************************************
      * LAYOUT...:                                                     *
      * - CHAVE GFCT....: GFCTB0E3 - SUSP_GRP_CLI                      *
      * - CHAVE GOTF....: GOTFB0C4 - TSUSP_COBR_CLI                    *
      * - MENSAGEM......: MENSAGEM INFORMATIVA.                        *
      ******************************************************************
       01  CMTFWZKR-AREA.
           05 CMTFWZKR-CHAVE-GFCT.
              10 CMTFWZKR-CSERVC-TARIF         PIC  9(005).
              10 CMTFWZKR-CAGPTO-CTA           PIC  9(003).
              10 CMTFWZKR-DINIC-SUSP-COBR      PIC  X(010).
              10 CMTFWZKR-CSEQ-AGPTO-CTA       PIC  9(009).
              10 CMTFWZKR-CCGC-CPF             PIC  9(009).
              10 CMTFWZKR-CFLIAL-CGC           PIC  9(005).
              10 CMTFWZKR-CCTRL-CPF-CGC        PIC  9(002).
      *
           05 CMTFWZKR-CHAVE-GOTF.
              10 CMTFWZKR-CPSSOA-JURID-CONGL   PIC  9(010).
              10 CMTFWZKR-CGRP-CONTR-NEGOC     PIC  9(003).
              10 CMTFWZKR-CPRODT-OPER-DEFLT    PIC  9(008).
              10 CMTFWZKR-COPER-PRODT-SERVC    PIC  9(005).
              10 CMTFWZKR-CCONDC-ECONC         PIC  9(005).
              10 CMTFWZKR-CPSSOA               PIC  9(010).
      *
           05 CMTFWZKR-MENSAGEM                 PIC  X(070).
           05 FILLER                            PIC  X(046).
      *
