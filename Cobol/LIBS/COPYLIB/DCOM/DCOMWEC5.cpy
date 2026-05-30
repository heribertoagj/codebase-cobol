      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0C5: DB2PRD.TOPER_DESC_COML                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4C5S                                       *
      *    -> FETCH   - DCOM4C5F                                       *
      *    -> INSERT  - DCOM4C5I                                       *
      *    -> UPDATE  - DCOM4C5U                                       *
      *    -> DELETE  - DCOM4C5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWC5E-HEADER.
          10 DCOMWC5E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWC5E'.
          10 DCOMWC5E-TAM-LAYOUT       PIC  9(005) VALUE 02100.
      *
       05 DCOMWC5E-BLOCO-ENTRADA.
          10 DCOMWC5E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWC5E-ENTRADA.
             15 DCOMWC5E-DANO-OPER-DESC            PIC S9(004) COMP-3.
             15 DCOMWC5E-NSEQ-OPER-DESC            PIC S9(009) COMP-3.
             15 DCOMWC5E-CSIT-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWC5E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMWC5E-VLIBRC-DESC-COML          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-VLIBRC-ANTCP-OPER         PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWC5E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWC5E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWC5E-CBCO-OPER-OPER            PIC S9(003) COMP-3.
             15 DCOMWC5E-CAG-OPER-OPER             PIC S9(005) COMP-3.
             15 DCOMWC5E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWC5E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWC5E-CTPO-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWC5E-CCONTR-CONVE-DESC         PIC S9(009) COMP-3.
             15 DCOMWC5E-CVRSAO-CONTR-CONVE        PIC S9(003) COMP-3.
             15 DCOMWC5E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWC5E-NSMULA-OPER-DESC          PIC S9(009) COMP-3.
             15 DCOMWC5E-CINDCD-DIGTC-AG           PIC  X(001).
             15 DCOMWC5E-CINDCD-ISENC-OPER         PIC  X(001).
             15 DCOMWC5E-CINDCD-AUTRZ-OPER         PIC  X(001).
             15 DCOMWC5E-CINDCD-MODLD-INCL         PIC  X(001).
             15 DCOMWC5E-CCTRO-CUSTO               PIC  X(004).
             15 DCOMWC5E-DANO-BASE                 PIC S9(004) COMP-3.
             15 DCOMWC5E-CDEPDC                    PIC S9(005) COMP-3.
             15 DCOMWC5E-CSEQ-STUDO                PIC S9(009) COMP-3.
             15 DCOMWC5E-CINDCD-APROV-CREDT        PIC  X(001).
             15 DCOMWC5E-VOPER-DESC-COML           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-DINIC-OPER-DESC           PIC  X(010).
             15 DCOMWC5E-DVCTO-FNAL-OPER           PIC  X(010).
             15 DCOMWC5E-TOPER-DESC-COML           PIC S9(005) COMP-3.
             15 DCOMWC5E-TMED-PONDE-OPER           PIC S9(003) COMP-3.
             15 DCOMWC5E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMWC5E-QPCELA-OPER               PIC S9(005) COMP-3.
             15 DCOMWC5E-QTITLO-CHEQ-PCELA         PIC S9(009) COMP-3.
             15 DCOMWC5E-NAUTRZ-TRNSM-ESCR         PIC S9(007) COMP-3.
             15 DCOMWC5E-CINDCD-PGDOR-JURO         PIC  X(001).
             15 DCOMWC5E-CTPO-TX-JURO-DESC         PIC S9(003) COMP-3.
             15 DCOMWC5E-PMIN-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWC5E-PMAX-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWC5E-PALIQT-IOF-DESC           PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWC5E-PTX-JURO-MES              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWC5E-PTX-JURO-ANO              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWC5E-PTX-CUSTO-MES             PIC S9(003)V9(006)
                                                               COMP-3.
             15 DCOMWC5E-PTX-CUSTO-ANO             PIC S9(003)V9(006)
                                                               COMP-3.
             15 DCOMWC5E-VJURO-OPER-DESC           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-VJURO-CUSTO-OPER          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-VTARIF-REG-TITLO          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-VTAC-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-VPSENT-OPER-DESC          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-VIOF-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-VLIQ-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWC5E-CINDCD-VALDC-ROTRO        PIC  X(001).
             15 DCOMWC5E-DVALDC-ROTRO-ELMTO        PIC  X(010).
             15 DCOMWC5E-CFUNC-VALDC-ROTRO         PIC S9(009) COMP-3.
             15 DCOMWC5E-DULT-ADTTO-OPER           PIC  X(010).
             15 DCOMWC5E-CINDCD-MOTVO-ADTTO        PIC  X(001).
             15 DCOMWC5E-HULT-ATULZ                PIC  X(026).
             15 DCOMWC5E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWC5E-CTERM                     PIC  X(008).
             15 DCOMWC5E-CCONTR-CORP-OPER          PIC S9(007) COMP-3.
             15 DCOMWC5E-DINCL-REG                 PIC  X(010).
             15 DCOMWC5E-CCART-LIM-CREDT           PIC  X(005).
             15 DCOMWC5E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMWC5E-CINDCD-ALIQT-REDZD        PIC  X(001).
             15 DCOMWC5E-PTX-JURO-DIA              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWC5E-CINDCD-FREQ-TX            PIC  X(002).
             15 DCOMWC5E-FILLER                    PIC  X(492).
      *
          10 DCOMWC5E-BLOCO-VARIAVEL.
             15 DCOMWC5E-V-CCNPJ-CPF               PIC  9(009).
             15 DCOMWC5E-V-CFLIAL-CNPJ             PIC  9(004).
             15 DCOMWC5E-V-CCTRL-CNPJ-CPF          PIC  9(002).
             15 DCOMWC5E-V-FILLER                  PIC  X(285).
      *
          10 DCOMWC5E-BLOCO-PAGINACAO.
             15 DCOMWC5E-MAX-OCORR                 PIC  9(003).
             15 DCOMWC5E-TOTALIZAR                 PIC  X(001).
             15 DCOMWC5E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWC5E-P-INICIAL              VALUE 'I'.
                88 DCOMWC5E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWC5E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWC5E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWC5E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWC5E-BLOCO-RESTART.
             15 DCOMWC5E-CHAVE-INI.
                20 DCOMWC5E-I-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWC5E-I-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWC5E-I-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC5E-I-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWC5E-I-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWC5E-I-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWC5E-I-CBCO-OPER-OPER       PIC S9(003) COMP-3.
                20 DCOMWC5E-I-CAG-OPER-OPER        PIC S9(005) COMP-3.
                20 DCOMWC5E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWC5E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWC5E-I-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC5E-I-CCONTR-CONVE-DESC    PIC S9(009) COMP-3.
                20 DCOMWC5E-I-CVRSAO-CONTR-CONVE   PIC S9(003) COMP-3.
                20 DCOMWC5E-I-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWC5E-I-FILLER               PIC  X(189).
             15 DCOMWC5E-CHAVE-FIM.
                20 DCOMWC5E-F-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWC5E-F-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWC5E-F-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC5E-F-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWC5E-F-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWC5E-F-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWC5E-F-CBCO-OPER-OPER       PIC S9(003) COMP-3.
                20 DCOMWC5E-F-CAG-OPER-OPER        PIC S9(005) COMP-3.
                20 DCOMWC5E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWC5E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWC5E-F-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC5E-F-CCONTR-CONVE-DESC    PIC S9(009) COMP-3.
                20 DCOMWC5E-F-CVRSAO-CONTR-CONVE   PIC S9(003) COMP-3.
                20 DCOMWC5E-F-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWC5E-I-FILLER               PIC  X(189).
      *
          10 DCOMWC5E-FILLER                       PIC  X(336).
      *
