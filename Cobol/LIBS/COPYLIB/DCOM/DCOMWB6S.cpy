      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWB6S-HEADER.
          10 DCOMWB6S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWB6S'.
          10 DCOMWB6S-TAM-LAYOUT       PIC  9(005) VALUE 19000.
      *
          10 DCOMWB6S-BLOCO-SAIDA.
             15 DCOMWB6S-QTDE-TOTAL                PIC  9(010).
             15 DCOMWB6S-QTDE-RETORNADA            PIC  9(003).
             15 DCOMWB6S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWB6S-DANO-OPER-DESC         PIC S9(004) COMP-3.
                20 DCOMWB6S-NSEQ-OPER-DESC         PIC S9(009) COMP-3.
                20 DCOMWB6S-HULT-ATULZ             PIC  X(026).
                20 DCOMWB6S-CSIT-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWB6S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMWB6S-VLIBRC-DESC-COML       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-VLIBRC-ANTCP-OPER      PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-CBCO                   PIC S9(003) COMP-3.
                20 DCOMWB6S-CAG-BCRIA              PIC S9(005) COMP-3.
                20 DCOMWB6S-CCTA-BCRIA-CLI         PIC S9(013) COMP-3.
                20 DCOMWB6S-CBCO-OPER-OPER         PIC S9(003) COMP-3.
                20 DCOMWB6S-CAG-OPER-OPER          PIC S9(005) COMP-3.
                20 DCOMWB6S-CPRODT                 PIC S9(003) COMP-3.
                20 DCOMWB6S-CSPROD-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWB6S-CTPO-DESC-COML         PIC S9(003) COMP-3.
                20 DCOMWB6S-CCONTR-CONVE-DESC      PIC S9(009) COMP-3.
                20 DCOMWB6S-CVRSAO-CONTR-CONVE     PIC S9(003) COMP-3.
                20 DCOMWB6S-CELMTO-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWB6S-NSMULA-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWB6S-CINDCD-DIGTC-AG        PIC  X(001).
                20 DCOMWB6S-CINDCD-ISENC-OPER      PIC  X(001).
                20 DCOMWB6S-CINDCD-AUTRZ-OPER      PIC  X(001).
                20 DCOMWB6S-CINDCD-MODLD-INCL      PIC  X(001).
                20 DCOMWB6S-CCTRO-CUSTO            PIC  X(004).
                20 DCOMWB6S-DANO-BASE              PIC S9(004) COMP-3.
                20 DCOMWB6S-CDEPDC                 PIC S9(005) COMP-3.
                20 DCOMWB6S-CSEQ-STUDO             PIC S9(009) COMP-3.
                20 DCOMWB6S-CINDCD-APROV-CREDT     PIC  X(001).
                20 DCOMWB6S-VOPER-DESC-COML        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-DINIC-OPER-DESC        PIC  X(010).
                20 DCOMWB6S-DVCTO-FNAL-OPER        PIC  X(010).
                20 DCOMWB6S-TOPER-DESC-COML        PIC S9(005) COMP-3.
                20 DCOMWB6S-TMED-PONDE-OPER        PIC S9(003) COMP-3.
                20 DCOMWB6S-CMEIO-ENTRD-DESC       PIC S9(003) COMP-3.
                20 DCOMWB6S-QPCELA-OPER            PIC S9(005) COMP-3.
                20 DCOMWB6S-QTITLO-CHEQ-PCELA      PIC S9(009) COMP-3.
                20 DCOMWB6S-NAUTRZ-TRNSM-ESCR      PIC S9(007) COMP-3.
                20 DCOMWB6S-CINDCD-PGDOR-JURO      PIC  X(001).
                20 DCOMWB6S-CTPO-TX-JURO-DESC      PIC S9(003) COMP-3.
                20 DCOMWB6S-PMIN-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWB6S-PMAX-TX-JURO-REGRA     PIC S9(003)V9(003)
                                                               COMP-3.
                20 DCOMWB6S-PALIQT-IOF-DESC        PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWB6S-PTX-JURO-MES           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWB6S-PTX-JURO-ANO           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWB6S-PTX-CUSTO-MES          PIC S9(003)V9(006)
                                                               COMP-3.
                20 DCOMWB6S-PTX-CUSTO-ANO          PIC S9(003)V9(006)
                                                               COMP-3.
                20 DCOMWB6S-VJURO-OPER-DESC        PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-VJURO-CUSTO-OPER       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-VTARIF-REG-TITLO       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-VTAC-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-VPSENT-OPER-DESC       PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-VIOF-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-VLIQ-OPER-DESC         PIC S9(015)V9(002)
                                                               COMP-3.
                20 DCOMWB6S-CINDCD-VALDC-ROTRO     PIC  X(001).
                20 DCOMWB6S-DVALDC-ROTRO-ELMTO     PIC  X(010).
                20 DCOMWB6S-CFUNC-VALDC-ROTRO      PIC S9(009) COMP-3.
                20 DCOMWB6S-DULT-ADTTO-OPER        PIC  X(010).
                20 DCOMWB6S-CINDCD-MOTVO-ADTTO     PIC  X(001).
                20 DCOMWB6S-CFUNC-BDSCO            PIC S9(009) COMP-3.
                20 DCOMWB6S-CTERM                  PIC  X(008).
                20 DCOMWB6S-CCONTR-CORP-OPER       PIC S9(007) COMP-3.
                20 DCOMWB6S-DINCL-REG              PIC  X(010).
                20 DCOMWB6S-CCART-LIM-CREDT        PIC  X(005).
                20 DCOMWB6S-CINDCD-ALIQT-REDZD     PIC  X(001).
                20 DCOMWB6S-PTX-JURO-DIA           PIC S9(003)V9(005)
                                                               COMP-3.
                20 DCOMWB6S-CINDCD-FREQ-TX         PIC  X(002).
             15 DCOMWB6S-FILLER                    PIC  X(1424).
      *================================================================*
