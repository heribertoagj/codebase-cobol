      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE ENTRADA:                 *
      *    -> DCOMB0G5: DB2PRD.TTRASI_PCELA_OPER                       *
      *----------------------------------------------------------------*
      * MODULOS DE ACESSO A BASE DE DADOS:                             *
      *    -> SELECT  - DCOM4G5S                                       *
      *    -> FETCH   - DCOM4G5F                                       *
      *    -> INSERT  - DCOM4G5I                                       *
      *    -> UPDATE  - DCOM4G5U                                       *
      *    -> DELETE  - DCOM4G5D                                       *
      *================================================================*
      *                                                                *
       05 DCOMWG5E-HEADER.
          10 DCOMWG5E-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG5E'.
          10 DCOMWG5E-TAM-LAYOUT       PIC  9(005) VALUE 01800.
      *
       05 DCOMWG5E-BLOCO-ENTRADA.
          10 DCOMWG5E-INSTRUCAO                 PIC  X(010).
      *
          10 DCOMWG5E-ENTRADA.
             15 DCOMWG5E-DANO-OPER-DESC         PIC S9(004)      COMP-3.
             15 DCOMWG5E-NSEQ-OPER-DESC         PIC S9(009)      COMP-3.
             15 DCOMWG5E-NPCELA-DESC-COML       PIC S9(005)      COMP-3.
             15 DCOMWG5E-CSIT-DESC-COML         PIC S9(003)      COMP-3.
             15 DCOMWG5E-HSIT-DESC-COML         PIC  X(026).
             15 DCOMWG5E-DVCTO-PCELA-DESC       PIC  X(010).
             15 DCOMWG5E-VPRINC-PCELA-DESC      PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-QTITLO-CHEQ-PCELA      PIC S9(009)      COMP-3.
             15 DCOMWG5E-PTX-JURO-MES           PIC S9(003)V9(5) COMP-3.
             15 DCOMWG5E-PTX-JURO-ANO           PIC S9(003)V9(5) COMP-3.
             15 DCOMWG5E-PTX-CUSTO-MES          PIC S9(003)V9(6) COMP-3.
             15 DCOMWG5E-PTX-CUSTO-ANO          PIC S9(003)V9(6) COMP-3.
             15 DCOMWG5E-VCUSTO-PCELA-DESC      PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VJURO-PCELA-DESC       PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VJURO-DIA-PCELA        PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VIOF-PCELA-DESC        PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VPSENT-PCELA-DESC      PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VJURO-APROP-ACUM       PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-DULT-APROP-PCELA       PIC  X(010).
             15 DCOMWG5E-PALIQT-IOF-PRROG       PIC S9(003)V9(5) COMP-3.
             15 DCOMWG5E-VIOF-PRROG-PCELA       PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-PCUSTO-MES-MORA        PIC S9(003)V9(6) COMP-3.
             15 DCOMWG5E-PCUSTO-ANO-MORA        PIC S9(003)V9(6) COMP-3.
             15 DCOMWG5E-PCUSTO-MES-CRATS       PIC S9(003)V9(6) COMP-3.
             15 DCOMWG5E-PCUSTO-ANO-CRATS       PIC S9(003)V9(6) COMP-3.
             15 DCOMWG5E-PALIQT-IOF-VENCD       PIC S9(003)V9(5) COMP-3.
             15 DCOMWG5E-VIOF-PCELA-VENCD       PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VJURO-MORA-GERC        PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VJURO-MORA-LEGAL       PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VSDO-DVDOR-PRINC       PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VCUSTO-PCELA-VENCD     PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VJURO-TRNSF-CRATS      PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VCUSTO-TRNSF-CRATS     PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-DTRNSF-CRATS-PCELA     PIC  X(010).
             15 DCOMWG5E-DTRNSF-LP-PCELA        PIC  X(010).
             15 DCOMWG5E-CBACEN-DESC-RURAL      PIC S9(009)      COMP-3.
             15 DCOMWG5E-CCONTR-CORP-PCELA      PIC S9(007)      COMP-3.
             15 DCOMWG5E-DULT-ADTTO-PCELA       PIC  X(010).
             15 DCOMWG5E-CINDCD-MOTVO-ADTTO     PIC  X(001).
             15 DCOMWG5E-CINDCD-PEND-EXTER      PIC  X(001).
             15 DCOMWG5E-CINDCD-BLOQ-CONS       PIC  X(001).
             15 DCOMWG5E-DBLOQ-CONS-CCORR       PIC  X(010).
             15 DCOMWG5E-DULT-PRROG-PCELA       PIC  X(010).
             15 DCOMWG5E-DULT-REATV-PCELA       PIC  X(010).
             15 DCOMWG5E-HULT-ATULZ             PIC  X(026).
             15 DCOMWG5E-CFUNC-BDSCO            PIC S9(009)      COMP-3.
             15 DCOMWG5E-CTERM                  PIC  X(008).
             15 DCOMWG5E-DINCL-REG              PIC  X(010).
             15 DCOMWG5E-VJURO-REMUN-VENCD      PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VMORA-VENCD-DESC       PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-VMULTA-VENCD-DESC      PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5E-FILLER                 PIC  X(492).
      *
          10 DCOMWG5E-BLOCO-VARIAVEL.
             15 DCOMWG5E-V-FILLER                  PIC  X(300).
      *
          10 DCOMWG5E-BLOCO-PAGINACAO.
             15 DCOMWG5E-MAX-OCORR                 PIC  9(003).
             15 DCOMWG5E-TOTALIZAR                 PIC  X(001).
             15 DCOMWG5E-INDICADOR-PAGINACAO       PIC  X(001).
                88 DCOMWG5E-P-INICIAL              VALUE 'I'.
                88 DCOMWG5E-P-PRIMEIRA             VALUE 'P'.
                88 DCOMWG5E-P-SEGUINTE             VALUE 'S'.
                88 DCOMWG5E-P-ANTERIOR             VALUE 'A'.
                88 DCOMWG5E-P-ULTIMA               VALUE 'U'.
      *
          10 DCOMWG5E-BLOCO-RESTART.
             15 DCOMWG5E-CHAVE-INI.
                20 DCOMWG5E-I-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWG5E-I-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWG5E-I-NPCELA-DESC-COML     PIC S9(005) COMP-3.
                20 DCOMWG5E-I-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG5E-I-DVCTO-PCELA-DESC     PIC  X(010).
                20 DCOMWG5E-I-FILLER               PIC  X(077).
             15 DCOMWG5E-CHAVE-FIM.
                20 DCOMWG5E-F-DANO-OPER-DESC       PIC S9(004) COMP-3.
                20 DCOMWG5E-F-NSEQ-OPER-DESC       PIC S9(009) COMP-3.
                20 DCOMWG5E-F-NPCELA-DESC-COML     PIC S9(005) COMP-3.
                20 DCOMWG5E-F-CSIT-DESC-COML       PIC S9(003) COMP-3.
                20 DCOMWG5E-F-DVCTO-PCELA-DESC     PIC  X(010).
                20 DCOMWG5E-F-FILLER               PIC  X(077).
      *
          10 DCOMWG5E-FILLER                       PIC  X(383).
      *
