      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0G4: DB2PRD.TTRASI_OPER_DESC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4G4S                                       *
      *    -> FETCH   - DCOM4G4F                                       *
      *    -> INSERT  - DCOM4G4I                                       *
      *    -> UPDATE  - DCOM4G4U                                       *
      *    -> DELETE  - DCOM4G4D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWG4E-HEADER.
          10 DCOMWG4E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG4E'.
          10 DCOMWG4E-TAM-LAYOUT       PIC  9(005) VALUE 02100.
      *
       05 DCOMWG4E-BLOCO-ENTRADA.
          10 DCOMWG4E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWG4E-ENTRADA.
             15 DCOMWG4E-DANO-OPER-DESC            PIC S9(004) COMP-3.
             15 DCOMWG4E-NSEQ-OPER-DESC            PIC S9(009) COMP-3.
             15 DCOMWG4E-CSIT-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWG4E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMWG4E-VLIBRC-DESC-COML          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-VLIBRC-ANTCP-OPER         PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-CBCO                      PIC S9(003) COMP-3.
             15 DCOMWG4E-CAG-BCRIA                 PIC S9(005) COMP-3.
             15 DCOMWG4E-CCTA-BCRIA-CLI            PIC S9(013) COMP-3.
             15 DCOMWG4E-CBCO-OPER-OPER            PIC S9(003) COMP-3.
             15 DCOMWG4E-CAG-OPER-OPER             PIC S9(005) COMP-3.
             15 DCOMWG4E-CPRODT                    PIC S9(003) COMP-3.
             15 DCOMWG4E-CSPROD-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWG4E-CTPO-DESC-COML            PIC S9(003) COMP-3.
             15 DCOMWG4E-CCONTR-CONVE-DESC         PIC S9(009) COMP-3.
             15 DCOMWG4E-CVRSAO-CONTR-CONVE        PIC S9(003) COMP-3.
             15 DCOMWG4E-CELMTO-DESC-COML          PIC S9(003) COMP-3.
             15 DCOMWG4E-NSMULA-OPER-DESC          PIC S9(009) COMP-3.
             15 DCOMWG4E-CINDCD-DIGTC-AG           PIC  X(001).
             15 DCOMWG4E-CINDCD-ISENC-OPER         PIC  X(001).
             15 DCOMWG4E-CINDCD-AUTRZ-OPER         PIC  X(001).
             15 DCOMWG4E-CINDCD-MODLD-INCL         PIC  X(001).
             15 DCOMWG4E-CCTRO-CUSTO               PIC  X(004).
             15 DCOMWG4E-DANO-BASE                 PIC S9(004) COMP-3.
             15 DCOMWG4E-CDEPDC                    PIC S9(005) COMP-3.
             15 DCOMWG4E-CSEQ-STUDO                PIC S9(009) COMP-3.
             15 DCOMWG4E-CINDCD-APROV-CREDT        PIC  X(001).
             15 DCOMWG4E-VOPER-DESC-COML           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-DINIC-OPER-DESC           PIC  X(010).
             15 DCOMWG4E-DVCTO-FNAL-OPER           PIC  X(010).
             15 DCOMWG4E-VOPER-DESC-APURA          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-QTITLO-OPER-APURA         PIC S9(005) COMP-3.
             15 DCOMWG4E-DVCTO-FNAL-APURA          PIC  X(010).
             15 DCOMWG4E-TOPER-DESC-APURA          PIC S9(003) COMP-3.
             15 DCOMWG4E-TMED-PONDE-OPER           PIC S9(003) COMP-3.
             15 DCOMWG4E-CMEIO-ENTRD-DESC          PIC S9(003) COMP-3.
             15 DCOMWG4E-QPCELA-OPER               PIC S9(005) COMP-3.
             15 DCOMWG4E-QTITLO-CHEQ-PCELA         PIC S9(009) COMP-3.
             15 DCOMWG4E-NAUTRZ-TRNSM-ESCR         PIC S9(007) COMP-3.
             15 DCOMWG4E-CINDCD-PGDOR-JURO         PIC  X(001).
             15 DCOMWG4E-CTPO-TX-JURO-DESC         PIC S9(003) COMP-3.
             15 DCOMWG4E-PMIN-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWG4E-PMAX-TX-JURO-REGRA        PIC S9(003)V9(003)
                                                               COMP-3.
             15 DCOMWG4E-PALIQT-IOF-DESC           PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWG4E-PTX-JURO-MES              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWG4E-PTX-JURO-ANO              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWG4E-PTX-CUSTO-MES             PIC S9(003)V9(006)
                                                               COMP-3.
             15 DCOMWG4E-PTX-CUSTO-ANO             PIC S9(003)V9(006)
                                                               COMP-3.
             15 DCOMWG4E-VJURO-OPER-DESC           PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-VJURO-CUSTO-OPER          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-VTARIF-REG-TITLO          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-VTAC-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-VPSENT-OPER-DESC          PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-VIOF-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-VLIQ-OPER-DESC            PIC S9(015)V9(002)
                                                               COMP-3.
             15 DCOMWG4E-CINDCD-VALDC-ROTRO        PIC  X(001).
             15 DCOMWG4E-DVALDC-ROTRO-ELMTO        PIC  X(010).
             15 DCOMWG4E-CFUNC-VALDC-ROTRO         PIC S9(009) COMP-3.
             15 DCOMWG4E-DULT-ADTTO-OPER           PIC  X(010).
             15 DCOMWG4E-CINDCD-MOTVO-ADTTO        PIC  X(001).
             15 DCOMWG4E-HULT-ATULZ                PIC  X(026).
             15 DCOMWG4E-CFUNC-BDSCO               PIC S9(009) COMP-3.
             15 DCOMWG4E-CTERM                     PIC  X(008).
             15 DCOMWG4E-CCONTR-CORP-OPER          PIC S9(007) COMP-3.
             15 DCOMWG4E-DINCL-REG                 PIC  X(010).
             15 DCOMWG4E-CCART-LIM-CREDT           PIC  X(005).
             15 DCOMWG4E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMWG4E-CESPCE-TITLO-COBR         PIC S9(003) COMP-3.
             15 DCOMWG4E-CINDCD-ALIQT-REDZD        PIC  X(001).
             15 DCOMWG4E-PTX-JURO-DIA              PIC S9(003)V9(005)
                                                               COMP-3.
             15 DCOMWG4E-CINDCD-FREQ-TX            PIC  X(002).
             15 DCOMWG4E-FILLER                    PIC  X(492).
      *
          10 DCOMWG4E-BLOCO-VARIAVEL.
             15 DCOMWG4E-V-CCNPJ-CPF               PIC  9(009).
             15 DCOMWG4E-V-CFLIAL-CNPJ             PIC  9(004).
             15 DCOMWG4E-V-CCTRL-CNPJ-CPF          PIC  9(002).
             15 DCOMWG4E-V-FILLER                  PIC  X(285).
      *
          10 DCOMWG4E-BLOCO-PAGINACAO.
             15 DCOMWG4E-MAX-OCORR                 PIC  9(003).
             15 DCOMWG4E-TOTALIZAR                 PIC  X(001).
             15 DCOMWG4E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWG4E-P-INICIAL              VALUE 'I'.
                88 DCOMWG4E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWG4E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWG4E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWG4E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWG4E-BLOCO-RESTART.
             15 DCOMWG4E-CHAVE-INI.
                20 DCOMWG4E-I-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWG4E-I-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWG4E-I-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG4E-I-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWG4E-I-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWG4E-I-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWG4E-I-CBCO-OPER-OPER       PIC S9(003) COMP-3.
                20 DCOMWG4E-I-CAG-OPER-OPER        PIC S9(005) COMP-3.
                20 DCOMWG4E-I-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWG4E-I-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWG4E-I-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG4E-I-CCONTR-CONVE-DESC    PIC S9(009) COMP-3.
                20 DCOMWG4E-I-CVRSAO-CONTR-CONVE   PIC S9(003) COMP-3.
                20 DCOMWG4E-I-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWG4E-I-FILLER               PIC  X(189).
             15 DCOMWG4E-CHAVE-FIM.
                20 DCOMWG4E-F-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWG4E-F-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWG4E-F-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG4E-F-CBCO                 PIC S9(003) COMP-3.
                20 DCOMWG4E-F-CAG-BCRIA            PIC S9(005) COMP-3.
                20 DCOMWG4E-F-CCTA-BCRIA-CLI       PIC S9(013) COMP-3.
                20 DCOMWG4E-F-CBCO-OPER-OPER       PIC S9(003) COMP-3.
                20 DCOMWG4E-F-CAG-OPER-OPER        PIC S9(005) COMP-3.
                20 DCOMWG4E-F-CPRODT               PIC S9(003) COMP-3.
                20 DCOMWG4E-F-CSPROD-DESC-COML     PIC S9(003) COMP-3.
                20 DCOMWG4E-F-CTPO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG4E-F-CCONTR-CONVE-DESC    PIC S9(009) COMP-3.
                20 DCOMWG4E-F-CVRSAO-CONTR-CONVE   PIC S9(003) COMP-3.
                20 DCOMWG4E-F-CCHAVE-ELMTO-DESC    PIC  X(045).
                20 DCOMWG4E-F-FILLER               PIC  X(189).
      *
          10 DCOMWG4E-FILLER                       PIC  X(313).
      *
