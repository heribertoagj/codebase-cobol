      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWC5S-HEADER.
          10 DCOMWC5S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWC5S'.
          10 DCOMWC5S-TAM-LAYOUT       PIC  9(005) VALUE 21000.
      *
          10 DCOMWC5S-BLOCO-SAIDA.
             15 DCOMWC5S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWC5S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWC5S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWC5S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWC5S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWC5S-CSIT-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWC5S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMWC5S-VLIBRC-DESC-COML       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-VLIBRC-ANTCP-OPER      PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWC5S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWC5S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWC5S-CBCO-OPER-OPER         PIC S9(003) COMP-3.
                20 DCOMWC5S-CAG-OPER-OPER          PIC S9(005) COMP-3.
                20 DCOMWC5S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMWC5S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC5S-CTPO-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWC5S-CCONTR-CONVE-DESC      PIC S9(009) COMP-3.
                20 DCOMWC5S-CVRSAO-CONTR-CONVE     PIC S9(003) COMP-3.
                20 DCOMWC5S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWC5S-NSMULA-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWC5S-CINDCD-DIGTC-AG        PIC  X(001).
                20 DCOMWC5S-CINDCD-ISENC-OPER      PIC  X(001).
                20 DCOMWC5S-CINDCD-AUTRZ-OPER      PIC  X(001).
                20 DCOMWC5S-CINDCD-MODLD-INCL      PIC  X(001).
                20 DCOMWC5S-CCTRO-CUSTO            PIC  X(004).
                20 DCOMWC5S-DANO-BASE              PIC S9(004) COMP-3.
                20 DCOMWC5S-CDEPDC                 PIC S9(005) COMP-3.
                20 DCOMWC5S-CSEQ-STUDO             PIC S9(009) COMP-3.
                20 DCOMWC5S-CINDCD-APROV-CREDT     PIC  X(001).
                20 DCOMWC5S-VOPER-DESC-COML        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-DINIC-OPER-DESC        PIC  X(010).
                20 DCOMWC5S-DVCTO-FNAL-OPER        PIC  X(010).
                20 DCOMWC5S-TOPER-DESC-COML        PIC S9(005) COMP-3.
                20 DCOMWC5S-TMED-PONDE-OPER        PIC S9(003) COMP-3.
                20 DCOMWC5S-CMEIO-ENTRD-DESC       PIC S9(003) COMP-3.
                20 DCOMWC5S-QPCELA-OPER            PIC S9(005) COMP-3.
                20 DCOMWC5S-QTITLO-CHEQ-PCELA      PIC S9(009) COMP-3.
                20 DCOMWC5S-NAUTRZ-TRNSM-ESCR      PIC S9(007) COMP-3.
                20 DCOMWC5S-CINDCD-PGDOR-JURO      PIC  X(001).
                20 DCOMWC5S-CTPO-TX-JURO-DESC      PIC S9(003) COMP-3.
                20 DCOMWC5S-PMIN-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWC5S-PMAX-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWC5S-PALIQT-IOF-DESC        PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWC5S-PTX-JURO-MES           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWC5S-PTX-JURO-ANO           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWC5S-PTX-CUSTO-MES          PIC S9(003)V9(006)
                                                               COMP-3.
                20 DCOMWC5S-PTX-CUSTO-ANO          PIC S9(003)V9(006)
                                                               COMP-3.
                20 DCOMWC5S-VJURO-OPER-DESC        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-VJURO-CUSTO-OPER       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-VTARIF-REG-TITLO       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-VTAC-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-VPSENT-OPER-DESC       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-VIOF-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-VLIQ-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWC5S-CINDCD-VALDC-ROTRO     PIC  X(001).
                20 DCOMWC5S-DVALDC-ROTRO-ELMTO     PIC  X(010).
                20 DCOMWC5S-CFUNC-VALDC-ROTRO      PIC S9(009) COMP-3.
                20 DCOMWC5S-DULT-ADTTO-OPER        PIC  X(010).
                20 DCOMWC5S-CINDCD-MOTVO-ADTTO     PIC  X(001).
                20 DCOMWC5S-HULT-ATULZ             PIC  X(026).
                20 DCOMWC5S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWC5S-CTERM                  PIC  X(008).
                20 DCOMWC5S-CCONTR-CORP-OPER       PIC S9(007) COMP-3.
                20 DCOMWC5S-DINCL-REG              PIC  X(010).
                20 DCOMWC5S-CCART-LIM-CREDT        PIC  X(005).
                20 DCOMWC5S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMWC5S-CINDCD-ALIQT-REDZD     PIC  X(001).
                20 DCOMWC5S-PTX-JURO-DIA           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWC5S-CINDCD-FREQ-TX         PIC  X(002).
             15 DCOMWC5S-FILLER                    PIC  X(1174).
      *================================================================*
