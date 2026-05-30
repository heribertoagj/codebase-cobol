      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0H8: DB2PRD.TTRASI_MEIO_LIBRC                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4H8S                                       *
      *    -> FETCH   - DCOM4H8F                                       *
      *    -> INSERT  - DCOM4H8I                                       *
      *    -> UPDATE  - DCOM4H8U                                       *
      *    -> DELETE  - DCOM4H8D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWH8S-HEADER.
          10 DCOMWH8S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWH8S'.
          10 DCOMWH8S-TAM-LAYOUT       PIC  9(005) VALUE 10000.
      *
          10 DCOMWH8S-BLOCO-SAIDA.
             15 DCOMWH8S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWH8S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWH8S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWH8S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWH8S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWH8S-CMEIO-LIBRC-DESC       PIC S9(003) COMP-3.
                20 DCOMWH8S-NOPER-MEIO-LIBRC       PIC S9(003) COMP-3.
                20 DCOMWH8S-CSIT-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWH8S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMWH8S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWH8S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWH8S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWH8S-CPOSTO-SERVC           PIC S9(003) COMP-3.
                20 DCOMWH8S-CCNPJ-CPF              PIC S9(009) COMP-3.
                20 DCOMWH8S-CFLIAL-CNPJ            PIC S9(005) COMP-3.
                20 DCOMWH8S-CCTRL-CNPJ-CPF         PIC S9(002) COMP-3.
                20 DCOMWH8S-IPSSOA-DESC-COML       PIC  X(060).
                20 DCOMWH8S-CCLUB                  PIC S9(010) COMP-3.
                20 DCOMWH8S-VLIBRC-DESC-COML       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWH8S-PLIBRC-DESC-COML       PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWH8S-DLIBRC-DESC-COML       PIC  X(010).
                20 DCOMWH8S-CINDCD-LIBRC-ANTCP     PIC  X(001).
                20 DCOMWH8S-HULT-ATULZ             PIC  X(026).
                20 DCOMWH8S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWH8S-CTERM                  PIC  X(008).
                20 DCOMWH8S-CINDCD-LIBRC-CANAL     PIC  X(001).
             15 DCOMWH8S-FILLER                    PIC  X(274).
      *                                                                *
      *================================================================*
