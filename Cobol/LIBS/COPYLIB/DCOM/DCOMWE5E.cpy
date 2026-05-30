      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
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
       05 DCOMWE5E-HEADER.
          10 DCOMWE5E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWE5E'.
          10 DCOMWE5E-TAM-LAYOUT       PIC  9(005) VALUE 00917.
      *
       05 DCOMWE5E-BLOCO-ENTRADA.
          10 DCOMWE5E-INSTRUCAO                    PIC  X(010).
      *
          10 DCOMWE5E-ENTRADA.
             15 DCOMWE5E-NSMULA-OPER-DESC          PIC  S9(009) COMP-3.
             15 DCOMWE5E-DINIC-OPER-DESC           PIC  X(010).
             15 DCOMWE5E-CSIT-DESC-COML            PIC  S9(003) COMP-3.
             15 DCOMWE5E-HSIT-DESC-COML            PIC  X(026).
             15 DCOMWE5E-CPRODT                    PIC  S9(003) COMP-3.
             15 DCOMWE5E-CSPROD-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMWE5E-CTPO-DESC-COML            PIC  S9(003) COMP-3.
             15 DCOMWE5E-CBCO                      PIC  S9(003) COMP-3.
             15 DCOMWE5E-CAG-BCRIA                 PIC  S9(005) COMP-3.
             15 DCOMWE5E-CCTA-BCRIA-CLI            PIC  S9(013) COMP-3.
             15 DCOMWE5E-CBCO-OPER-OPER            PIC  S9(003) COMP-3.
             15 DCOMWE5E-CAG-OPER-OPER             PIC  S9(005) COMP-3.
             15 DCOMWE5E-CCONTR-CONVE-DESC         PIC  S9(009) COMP-3.
             15 DCOMWE5E-CVRSAO-CONTR-CONVE        PIC  S9(003) COMP-3.
             15 DCOMWE5E-CELMTO-DESC-COML          PIC  S9(003) COMP-3.
             15 DCOMWE5E-CINDCD-DIGTC-AG           PIC  X(001).
             15 DCOMWE5E-CINDCD-MODLD-INCL         PIC  X(001).
             15 DCOMWE5E-CCTRO-CUSTO               PIC  X(004).
             15 DCOMWE5E-VOPER-DESC-COML           PIC  S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWE5E-TOPER-DESC-APURA          PIC  S9(003) COMP-3.
             15 DCOMWE5E-VOPER-DESC-APURA          PIC  S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWE5E-QTITLO-OPER-APURA         PIC  S9(005) COMP-3.
             15 DCOMWE5E-DVCTO-FNAL-APURA          PIC  X(010).
             15 DCOMWE5E-TMED-PONDE-OPER           PIC  S9(003) COMP-3.
             15 DCOMWE5E-CMEIO-ENTRD-DESC          PIC  S9(003) COMP-3.
             15 DCOMWE5E-CTPO-TX-JURO-DESC         PIC  S9(003) COMP-3.
             15 DCOMWE5E-PALIQT-IOF-DESC           PIC  S9(003)V9(005)
                                                                 COMP-3.
             15 DCOMWE5E-PTX-JURO-MES              PIC  S9(003)V9(005)
                                                                 COMP-3.
             15 DCOMWE5E-PTX-JURO-ANO              PIC  S9(003)V9(005)
                                                                 COMP-3.
             15 DCOMWE5E-VJURO-OPER-DESC           PIC  S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWE5E-VTARIF-REG-TITLO          PIC  S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWE5E-VTAC-OPER-DESC            PIC  S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWE5E-VIOF-OPER-DESC            PIC  S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWE5E-VLIQ-OPER-DESC            PIC  S9(015)V9(002)
                                                                 COMP-3.
             15 DCOMWE5E-HULT-ATULZ                PIC  X(026).
             15 DCOMWE5E-CFUNC-BDSCO               PIC  S9(009) COMP-3.
             15 DCOMWE5E-CTERM                     PIC  X(008).
             15 DCOMWE5E-DINCL-REG                 PIC  X(010).
             15 DCOMWE5E-CCHAVE-ELMTO-DESC         PIC  X(045).
             15 DCOMWE5E-PTX-CUSTO-SPREAD          PIC  S9(003)V9(003)
                                                                COMP-3.
             15 DCOMWE5E-QTITLO-CHEQ-PCELA         PIC  S9(009) COMP-3.
             15 DCOMWE5E-CINDCD-ALIQT-REDZD        PIC  X(001).
             15 DCOMWE5E-PTX-JURO-DIA              PIC  S9(003)V9(005)
                                                                COMP-3.
             15 DCOMWE5E-CINDCD-FREQ-TX            PIC  X(002).
             15 DCOMWE5E-DANO-BASE                 PIC  S9(004) COMP-3.
             15 DCOMWE5E-CDEPDC                    PIC  S9(005) COMP-3.
             15 DCOMWE5E-CSEQ-STUDO                PIC  S9(009) COMP-3.
             15 DCOMWE5E-CCART-LIM-CREDT           PIC  X(005).

          10 DCOMWE5E-BLOCO-PAGINACAO.
             15 DCOMWE5E-MAX-OCORR                 PIC  9(003).
             15 DCOMWE5E-TOTALIZAR                 PIC  X(001).
             15 DCOMWE5E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWE5E-P-INICIAL              VALUE 'I'.
                88 DCOMWE5E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWE5E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWE5E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWE5E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWE5E-BLOCO-RESTART.
             15 DCOMWE5E-CHAVE-INI.
                20 DCOMWE5E-I-NSMULA-OPER-DESC     PIC  S9(009) COMP-3.
                20 FILLER                          PIC  X(200).
             15 DCOMWE5E-CHAVE-FIM.
                20 DCOMWE5E-F-NSMULA-OPER-DESC     PIC  S9(009) COMP-3.
                20 FILLER                          PIC  X(200).
      *
          10 DCOMWE5E-FILLER                       PIC  X(172).
      *
