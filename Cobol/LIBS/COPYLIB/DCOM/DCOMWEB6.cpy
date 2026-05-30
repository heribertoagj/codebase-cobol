      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0B6: DB2PRD.THIST_OPER_DESC                         *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4B6S                                       *
      *    -> FETCH   - DCOM4B6F                                       *
      *    -> INSERT  - DCOM4B6I                                       *
      *    -> UPDATE  - DCOM4B6U                                       *
      *    -> DELETE  - DCOM4B6D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWB6E-HEADER.
          10 DCOMWB6E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWB6E'.
          10 DCOMWB6E-TAM-LAYOUT       PIC  9(005) VALUE 02100.
      *
       05 DCOMWB6E-BLOCO-ENTRADA.
          10 DCOMWB6E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWB6E-ENTRADA.
             15 DCOMWB6E-DANO-OPER-DESC            PIC S9(004) COMP-3.
             15 DCOMWB6E-NSEQ-OPER-DESC            PIC S9(009) COMP-3.
             15 DCOMWB6E-HULT-ATULZ                PIC  X(026).
             15 DCOMWB6E-CSIT-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWB6E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMWB6E-VLIBRC-DESC-COML          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-VLIBRC-ANTCP-OPER         PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWB6E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWB6E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWB6E-CBCO-OPER-OPER            PIC S9(003) COMP-3.
             15 DCOMWB6E-CAG-OPER-OPER             PIC S9(005) COMP-3.
             15 DCOMWB6E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWB6E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWB6E-CTPO-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWB6E-CCONTR-CONVE-DESC         PIC S9(009) COMP-3.
             15 DCOMWB6E-CVRSAO-CONTR-CONVE        PIC S9(003) COMP-3.
             15 DCOMWB6E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWB6E-NSMULA-OPER-DESC          PIC S9(009) COMP-3.
             15 DCOMWB6E-CINDCD-DIGTC-AG           PIC  X(001).
             15 DCOMWB6E-CINDCD-ISENC-OPER         PIC  X(001).
             15 DCOMWB6E-CINDCD-AUTRZ-OPER         PIC  X(001).
             15 DCOMWB6E-CINDCD-MODLD-INCL         PIC  X(001).
             15 DCOMWB6E-CCTRO-CUSTO               PIC  X(004).
             15 DCOMWB6E-DANO-BASE                 PIC S9(004) COMP-3.
             15 DCOMWB6E-CDEPDC                    PIC S9(005) COMP-3.
             15 DCOMWB6E-CSEQ-STUDO                PIC S9(009) COMP-3.
             15 DCOMWB6E-CINDCD-APROV-CREDT        PIC  X(001).
             15 DCOMWB6E-VOPER-DESC-COML           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-DINIC-OPER-DESC           PIC  X(010).
             15 DCOMWB6E-DVCTO-FNAL-OPER           PIC  X(010).
             15 DCOMWB6E-TOPER-DESC-COML           PIC S9(005) COMP-3.
             15 DCOMWB6E-TMED-PONDE-OPER           PIC S9(003) COMP-3.
             15 DCOMWB6E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMWB6E-QPCELA-OPER               PIC S9(005) COMP-3.
             15 DCOMWB6E-QTITLO-CHEQ-PCELA         PIC S9(009) COMP-3.
             15 DCOMWB6E-NAUTRZ-TRNSM-ESCR         PIC S9(007) COMP-3.
             15 DCOMWB6E-CINDCD-PGDOR-JURO         PIC  X(001).
             15 DCOMWB6E-CTPO-TX-JURO-DESC         PIC S9(003) COMP-3.
             15 DCOMWB6E-PMIN-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWB6E-PMAX-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWB6E-PALIQT-IOF-DESC           PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWB6E-PTX-JURO-MES              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWB6E-PTX-JURO-ANO              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWB6E-PTX-CUSTO-MES             PIC S9(003)V9(006)
                                                               COMP-3.
             15 DCOMWB6E-PTX-CUSTO-ANO             PIC S9(003)V9(006)
                                                               COMP-3.
             15 DCOMWB6E-VJURO-OPER-DESC           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-VJURO-CUSTO-OPER          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-VTARIF-REG-TITLO          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-VTAC-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-VPSENT-OPER-DESC          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-VIOF-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-VLIQ-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWB6E-CINDCD-VALDC-ROTRO        PIC  X(001).
             15 DCOMWB6E-DVALDC-ROTRO-ELMTO        PIC  X(010).
             15 DCOMWB6E-CFUNC-VALDC-ROTRO         PIC S9(009) COMP-3.
             15 DCOMWB6E-DULT-ADTTO-OPER           PIC  X(010).
             15 DCOMWB6E-CINDCD-MOTVO-ADTTO        PIC  X(001).
             15 DCOMWB6E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWB6E-CTERM                     PIC  X(008).
             15 DCOMWB6E-CCONTR-CORP-OPER          PIC S9(007) COMP-3.
             15 DCOMWB6E-DINCL-REG                 PIC  X(010).
             15 DCOMWB6E-CCART-LIM-CREDT           PIC  X(005).
             15 DCOMWB6E-CINDCD-ALIQT-REDZD        PIC  X(001).
             15 DCOMWB6E-PTX-JURO-DIA              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWB6E-CINDCD-FREQ-TX            PIC  X(002).
             15 DCOMWB6E-FILLER                    PIC  X(492).
      *
          10 DCOMWB6E-BLOCO-VARIAVEL.
             15 DCOMWB6E-V-CCNPJ-CPF               PIC  9(009).
             15 DCOMWB6E-V-CFLIAL-CNPJ             PIC  9(004).
             15 DCOMWB6E-V-CCTRL-CNPJ-CPF          PIC  9(002).
             15 DCOMWB6E-V-FILLER                  PIC  X(285).
      *
          10 DCOMWB6E-BLOCO-PAGINACAO.
             15 DCOMWB6E-MAX-OCORR                 PIC  9(003).
             15 DCOMWB6E-TOTALIZAR                 PIC  X(001).
             15 DCOMWB6E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWB6E-P-INICIAL              VALUE 'I'.
                88 DCOMWB6E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWB6E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWB6E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWB6E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWB6E-BLOCO-RESTART.
             15 DCOMWB6E-CHAVE-INI.
                20 DCOMWB6E-I-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWB6E-I-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWB6E-I-HULT-ATULZ           PIC  X(026).
                20 DCOMWB6E-I-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWB6E-I-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWB6E-I-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWB6E-I-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWB6E-I-CBCO-OPER-OPER       PIC S9(003) COMP-3.
                20 DCOMWB6E-I-CAG-OPER-OPER        PIC S9(005) COMP-3.
                20 DCOMWB6E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWB6E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWB6E-I-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWB6E-I-CCONTR-CONVE-DESC    PIC S9(009) COMP-3.
                20 DCOMWB6E-I-CVRSAO-CONTR-CONVE   PIC S9(003) COMP-3.
                20 DCOMWB6E-I-FILLER               PIC  X(208).
             15 DCOMWB6E-CHAVE-FIM.
                20 DCOMWB6E-F-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWB6E-F-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWB6E-F-HULT-ATULZ           PIC  X(026).
                20 DCOMWB6E-F-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWB6E-F-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWB6E-F-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWB6E-F-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWB6E-F-CBCO-OPER-OPER       PIC S9(003) COMP-3.
                20 DCOMWB6E-F-CAG-OPER-OPER        PIC S9(005) COMP-3.
                20 DCOMWB6E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWB6E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWB6E-F-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWB6E-F-CCONTR-CONVE-DESC    PIC S9(009) COMP-3.
                20 DCOMWB6E-F-CVRSAO-CONTR-CONVE   PIC S9(003) COMP-3.
                20 DCOMWB6E-F-FILLER               PIC  X(208).
      *
          10 DCOMWB6E-FILLER                       PIC  X(381).
      *
