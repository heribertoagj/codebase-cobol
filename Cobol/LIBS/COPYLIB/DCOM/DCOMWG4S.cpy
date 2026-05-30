      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWG4S-HEADER.
          10 DCOMWG4S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG4S'.
          10 DCOMWG4S-TAM-LAYOUT       PIC  9(005) VALUE 22000.
      *
          10 DCOMWG4S-BLOCO-SAIDA.
             15 DCOMWG4S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWG4S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWG4S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWG4S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWG4S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWG4S-CSIT-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWG4S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMWG4S-VLIBRC-DESC-COML       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-VLIBRC-ANTCP-OPER      PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWG4S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWG4S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWG4S-CBCO-OPER-OPER         PIC S9(003) COMP-3.
                20 DCOMWG4S-CAG-OPER-OPER          PIC S9(005) COMP-3.
                20 DCOMWG4S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMWG4S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG4S-CTPO-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWG4S-CCONTR-CONVE-DESC      PIC S9(009) COMP-3.
                20 DCOMWG4S-CVRSAO-CONTR-CONVE     PIC S9(003) COMP-3.
                20 DCOMWG4S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG4S-NSMULA-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWG4S-CINDCD-DIGTC-AG        PIC  X(001).
                20 DCOMWG4S-CINDCD-ISENC-OPER      PIC  X(001).
                20 DCOMWG4S-CINDCD-AUTRZ-OPER      PIC  X(001).
                20 DCOMWG4S-CINDCD-MODLD-INCL      PIC  X(001).
                20 DCOMWG4S-CCTRO-CUSTO            PIC  X(004).
                20 DCOMWG4S-DANO-BASE              PIC S9(004) COMP-3.
                20 DCOMWG4S-CDEPDC                 PIC S9(005) COMP-3.
                20 DCOMWG4S-CSEQ-STUDO             PIC S9(009) COMP-3.
                20 DCOMWG4S-CINDCD-APROV-CREDT     PIC  X(001).
                20 DCOMWG4S-VOPER-DESC-COML        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-DINIC-OPER-DESC        PIC  X(010).
                20 DCOMWG4S-DVCTO-FNAL-OPER        PIC  X(010).
                20 DCOMWG4S-VOPER-DESC-APURA       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-QTITLO-OPER-APURA      PIC S9(005) COMP-3.
                20 DCOMWG4S-DVCTO-FNAL-APURA       PIC  X(010).
                20 DCOMWG4S-TOPER-DESC-APURA       PIC S9(003) COMP-3.
                20 DCOMWG4S-TMED-PONDE-OPER        PIC S9(003) COMP-3.
                20 DCOMWG4S-CMEIO-ENTRD-DESC       PIC S9(003) COMP-3.
                20 DCOMWG4S-QPCELA-OPER            PIC S9(005) COMP-3.
                20 DCOMWG4S-QTITLO-CHEQ-PCELA      PIC S9(009) COMP-3.
                20 DCOMWG4S-NAUTRZ-TRNSM-ESCR      PIC S9(007) COMP-3.
                20 DCOMWG4S-CINDCD-PGDOR-JURO      PIC  X(001).
                20 DCOMWG4S-CTPO-TX-JURO-DESC      PIC S9(003) COMP-3.
                20 DCOMWG4S-PMIN-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWG4S-PMAX-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWG4S-PALIQT-IOF-DESC        PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWG4S-PTX-JURO-MES           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWG4S-PTX-JURO-ANO           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWG4S-PTX-CUSTO-MES          PIC S9(003)V9(006)
                                                               COMP-3.
                20 DCOMWG4S-PTX-CUSTO-ANO          PIC S9(003)V9(006)
                                                               COMP-3.
                20 DCOMWG4S-VJURO-OPER-DESC        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-VJURO-CUSTO-OPER       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-VTARIF-REG-TITLO       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-VTAC-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-VPSENT-OPER-DESC       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-VIOF-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-VLIQ-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWG4S-CINDCD-VALDC-ROTRO     PIC  X(001).
                20 DCOMWG4S-DVALDC-ROTRO-ELMTO     PIC  X(010).
                20 DCOMWG4S-CFUNC-VALDC-ROTRO      PIC S9(009) COMP-3.
                20 DCOMWG4S-DULT-ADTTO-OPER        PIC  X(010).
                20 DCOMWG4S-CINDCD-MOTVO-ADTTO     PIC  X(001).
                20 DCOMWG4S-HULT-ATULZ             PIC  X(026).
                20 DCOMWG4S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWG4S-CTERM                  PIC  X(008).
                20 DCOMWG4S-CCONTR-CORP-OPER       PIC S9(007) COMP-3.
                20 DCOMWG4S-DINCL-REG              PIC  X(010).
                20 DCOMWG4S-CCART-LIM-CREDT        PIC  X(005).
                20 DCOMWG4S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMWG4S-CESPCE-TITLO-COBR      PIC S9(003) COMP-3.
                20 DCOMWG4S-CINDCD-ALIQT-REDZD     PIC  X(001).
                20 DCOMWG4S-PTX-JURO-DIA           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWG4S-CINDCD-FREQ-TX         PIC  X(002).
             15 DCOMWG4S-FILLER                    PIC  X(1024).
      *                                                                *
      *================================================================*
