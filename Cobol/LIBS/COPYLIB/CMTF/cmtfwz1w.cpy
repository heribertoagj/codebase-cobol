      ******************************************************************
      * NOME BOOK : CMTFWZ1W                                           *
      * DESCRICAO : BOOK DE CONTROLE DO ESPELHAMENTO.                  *
      *             AREA REFERENTE A TABELA GFCTB0E7 - SUSP_TARIF_AGPTO*
      *                            E TABELA GFCTB0E3 - SUSP_GRP_CLI    *
      * DATA      : 25/10/2017                                         *
      * AUTOR     : GEORGIA                                            *
      * EMPRESA   : CAPGEMINI                                          *
      * TAMANHO   : 600                                                *
      ******************************************************************
      * AREA-LOG-ERRO.                                                 *
      *   TABELA .............: NOME DA TABELA CORRESPONDENTE AO       *
      *                         LAYOUT DESTE BOOK.                     *
      *   DATA-LOG ...........: DATA DE INCLUSAO DO LOG DE ERRO        *
      *                                                                *
      * AREA-ESPELHAMENTO.                                             *
      *   TIPO ESPELHAMENTO ..: TIPO DE ESPELHAMENTO                   *
      *                         "I" = INCLUSAO                         *
      *                         "A" = ALTERACAO                        *
      *                         "E" = EXCLUSAO                         *
      *   DATA ESPELHAMENTO ..: TIMESTAMP PARA ESPELHAMENTO            *
      ******************************************************************
       01  CMTFWZ1W-AREA.
           05 CMTFWZ1W-AREA-LOG.
              10 CMTFWZ1W-TABELA                   PIC  X(008).
              10 CMTFWZ1W-DATA-LOG                 PIC  X(010).
      *
           05 CMTFWZ1W-AREA-ESPELHAMENTO.
              10 CMTFWZ1W-TIPO-ESPELHAMENTO        PIC  X(001).
              10 CMTFWZ1W-DATA-ESPELHAMENTO        PIC  X(026).
      *
           05 CMTFWZ1W-AREA-REGISTRO.
              10 CMTFWZ1W-CSERVC-TARIF             PIC S9(005) COMP-3.
              10 CMTFWZ1W-CAGPTO-CTA               PIC S9(003) COMP-3.
              10 CMTFWZ1W-DINIC-SUSP-COBR          PIC  X(010).
              10 CMTFWZ1W-CSEQ-AGPTO-CTA           PIC S9(009) COMP-3.
              10 CMTFWZ1W-CFUNC-MANUT              PIC S9(009) COMP-3.
              10 CMTFWZ1W-CFUNC-MANUT-INCL         PIC S9(009) COMP-3.
              10 CMTFWZ1W-DFIM-SUSP-COBR           PIC  X(010).
              10 CMTFWZ1W-CINDCD-LIBRC-COBR        PIC  X(001).
              10 CMTFWZ1W-DCOBR-SUSP-LIBRD         PIC  X(010).
              10 CMTFWZ1W-RJUSTF-SUSP-COBR.
                 49 CMTFWZ1W-RJUSTF-SUSP-COBR-LEN  PIC S9(004) COMP.
                 49 CMTFWZ1W-RJUSTF-SUSP-COBR-TEXT PIC  X(255).
              10 CMTFWZ1W-CINDCD-AGPTO-TOT         PIC  X(001).
              10 CMTFWZ1W-HMANUT-REG               PIC  X(026).
              10 CMTFWZ1W-HINCL-REG-SIST           PIC  X(026).
              10 CMTFWZ1W-CCGC-CPF                 PIC S9(009) COMP-3.
              10 CMTFWZ1W-CFLIAL-CGC               PIC S9(005) COMP-3.
              10 CMTFWZ1W-CCTRL-CPF-CGC            PIC S9(002) COMP-3.
      *
           05 FILLER                               PIC  X(184).
      *
