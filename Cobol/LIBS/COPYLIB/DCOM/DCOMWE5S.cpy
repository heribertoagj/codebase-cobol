      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
      *    -> DCOMB0E5: DB2PRD.TSMULA_OPER_DESC                        *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4E5S                                       *
      *    -> FETCH   - DCOM4E5F                                       *
      *    -> INSERT  - DCOM4E5I                                       *
      *    -> UPDATE  - DCOM4E5U                                       *
      *    -> DELETE  - DCOM4E5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWE5S-HEADER.
          10 DCOMWE5S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE5S'.
          10 DCOMWE5S-TAM-LAYOUT       PIC  9(005) VALUE 15371.
      *
          10 DCOMWE5S-BLOCO-SAIDA.
             15 DCOMWE5S-QTDE-TOTAL                PIC S9(010) COMP-3.
             15 DCOMWE5S-QTDE-RETORNADA            PIC S9(003) COMP-3.
             15 DCOMWE5S-OCORRENCIA                OCCURS 050 TIMES.
                20 DCOMWE5S-NSMULA-OPER-DESC       PIC  S9(009) COMP-3.
                20 DCOMWE5S-DINIC-OPER-DESC        PIC  X(010).
                20 DCOMWE5S-CSIT-DESC-COML         PIC  S9(003) COMP-3.
                20 DCOMWE5S-HSIT-DESC-COML         PIC  X(026).
                20 DCOMWE5S-CPRODT                 PIC  S9(003) COMP-3.
                20 DCOMWE5S-CSPROD-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMWE5S-CTPO-DESC-COML         PIC  S9(003) COMP-3.
                20 DCOMWE5S-CBCO                   PIC  S9(003) COMP-3.
                20 DCOMWE5S-CAG-BCRIA              PIC  S9(005) COMP-3.
                20 DCOMWE5S-CCTA-BCRIA-CLI         PIC  S9(013) COMP-3.
                20 DCOMWE5S-CBCO-OPER-OPER         PIC  S9(003) COMP-3.
                20 DCOMWE5S-CAG-OPER-OPER          PIC  S9(005) COMP-3.
                20 DCOMWE5S-CCONTR-CONVE-DESC      PIC  S9(009) COMP-3.
                20 DCOMWE5S-CVRSAO-CONTR-CONVE     PIC  S9(003) COMP-3.
                20 DCOMWE5S-CELMTO-DESC-COML       PIC  S9(003) COMP-3.
                20 DCOMWE5S-CINDCD-DIGTC-AG        PIC  X(001).
                20 DCOMWE5S-CINDCD-MODLD-INCL      PIC  X(001).
                20 DCOMWE5S-CCTRO-CUSTO            PIC  X(004).
                20 DCOMWE5S-VOPER-DESC-COML        PIC  S9(015)V9(002)
                                                         COMP-3.
                20 DCOMWE5S-TOPER-DESC-APURA       PIC  S9(003) COMP-3.
                20 DCOMWE5S-VOPER-DESC-APURA       PIC  S9(015)V9(002)
                                                         COMP-3.
                20 DCOMWE5S-QTITLO-OPER-APURA      PIC  S9(005) COMP-3.
                20 DCOMWE5S-DVCTO-FNAL-APURA       PIC  X(010).
                20 DCOMWE5S-TMED-PONDE-OPER        PIC  S9(003) COMP-3.
                20 DCOMWE5S-CMEIO-ENTRD-DESC       PIC  S9(003) COMP-3.
                20 DCOMWE5S-CTPO-TX-JURO-DESC      PIC  S9(003) COMP-3.
                20 DCOMWE5S-PALIQT-IOF-DESC        PIC  S9(003)V9(005)
                                                         COMP-3.
                20 DCOMWE5S-PTX-JURO-MES           PIC  S9(003)V9(005)
                                                         COMP-3.
                20 DCOMWE5S-PTX-JURO-ANO           PIC  S9(003)V9(005)
                                                         COMP-3.
                20 DCOMWE5S-VJURO-OPER-DESC        PIC  S9(015)V9(002)
                                                         COMP-3.
                20 DCOMWE5S-VTARIF-REG-TITLO       PIC  S9(015)V9(002)
                                                         COMP-3.
                20 DCOMWE5S-VTAC-OPER-DESC         PIC  S9(015)V9(002)
                                                         COMP-3.
                20 DCOMWE5S-VIOF-OPER-DESC         PIC  S9(015)V9(002)
                                                         COMP-3.
                20 DCOMWE5S-VLIQ-OPER-DESC         PIC  S9(015)V9(002)
                                                         COMP-3.
                20 DCOMWE5S-HULT-ATULZ             PIC  X(026).
                20 DCOMWE5S-CFUNC-BDSCO            PIC  S9(009) COMP-3.
                20 DCOMWE5S-CTERM                  PIC  X(008).
                20 DCOMWE5S-DINCL-REG              PIC  X(010).
                20 DCOMWE5S-CCHAVE-ELMTO-DESC      PIC  X(045).
                20 DCOMWE5S-PTX-CUSTO-SPREAD       PIC  S9(003)V9(003)
                                                         COMP-3.
                20 DCOMWE5S-QTITLO-CHEQ-PCELA      PIC  S9(009) COMP-3.
                20 DCOMWE5S-CINDCD-ALIQT-REDZD     PIC  X(001).
                20 DCOMWE5S-PTX-JURO-DIA           PIC  S9(003)V9(005)
                                                         COMP-3.
                20 DCOMWE5S-CINDCD-FREQ-TX         PIC  X(002).
                20 DCOMWE5S-DANO-BASE              PIC  S9(004) COMP-3.
                20 DCOMWE5S-CDEPDC                 PIC  S9(005) COMP-3.
                20 DCOMWE5S-CSEQ-STUDO             PIC  S9(009) COMP-3.
                20 DCOMWE5S-CCART-LIM-CREDT        PIC  X(005).

