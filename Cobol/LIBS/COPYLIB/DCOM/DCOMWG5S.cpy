      *================================================================*
      * BOOK PARA ACESSO A BASE DB2 - AREA DE SAIDA:                   *
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
       05 DCOMWG5S-HEADER.
          10 DCOMWG5S-COD-LAYOUT       PIC  X(008) VALUE 'DCOMWG5S'.
          10 DCOMWG5S-TAM-LAYOUT       PIC  9(005) VALUE 22000.
      *
          10 DCOMWG5S-BLOCO-SAIDA.
             15 DCOMWG5S-QTDE-TOTAL             PIC  9(010).
             15 DCOMWG5S-QTDE-RETORNADA         PIC  9(003).
             15 DCOMWG5S-OCORRENCIA             OCCURS 050 TIMES.
                20 DCOMWG5S-DANO-OPER-DESC      PIC S9(004)      COMP-3.
                20 DCOMWG5S-NSEQ-OPER-DESC      PIC S9(009)      COMP-3.
                20 DCOMWG5S-NPCELA-DESC-COML    PIC S9(005)      COMP-3.
                20 DCOMWG5S-CSIT-DESC-COML      PIC S9(003)      COMP-3.
                20 DCOMWG5S-HSIT-DESC-COML      PIC  X(026).
                20 DCOMWG5S-DVCTO-PCELA-DESC    PIC  X(010).
                20 DCOMWG5S-VPRINC-PCELA-DESC   PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-QTITLO-CHEQ-PCELA   PIC S9(009)      COMP-3.
                20 DCOMWG5S-PTX-JURO-MES        PIC S9(003)V9(5) COMP-3.
                20 DCOMWG5S-PTX-JURO-ANO        PIC S9(003)V9(5) COMP-3.
                20 DCOMWG5S-PTX-CUSTO-MES       PIC S9(003)V9(6) COMP-3.
                20 DCOMWG5S-PTX-CUSTO-ANO       PIC S9(003)V9(6) COMP-3.
                20 DCOMWG5S-VCUSTO-PCELA-DESC   PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VJURO-PCELA-DESC    PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VJURO-DIA-PCELA     PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VIOF-PCELA-DESC     PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VPSENT-PCELA-DESC   PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VJURO-APROP-ACUM    PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-DULT-APROP-PCELA    PIC  X(010).
                20 DCOMWG5S-PALIQT-IOF-PRROG    PIC S9(003)V9(5) COMP-3.
                20 DCOMWG5S-VIOF-PRROG-PCELA    PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-PCUSTO-MES-MORA     PIC S9(003)V9(6) COMP-3.
                20 DCOMWG5S-PCUSTO-ANO-MORA     PIC S9(003)V9(6) COMP-3.
                20 DCOMWG5S-PCUSTO-MES-CRATS    PIC S9(003)V9(6) COMP-3.
                20 DCOMWG5S-PCUSTO-ANO-CRATS    PIC S9(003)V9(6) COMP-3.
                20 DCOMWG5S-PALIQT-IOF-VENCD    PIC S9(003)V9(5) COMP-3.
                20 DCOMWG5S-VIOF-PCELA-VENCD    PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VJURO-MORA-GERC     PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VJURO-MORA-LEGAL    PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VSDO-DVDOR-PRINC    PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VCUSTO-PCELA-VENCD  PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VJURO-TRNSF-CRATS   PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VCUSTO-TRNSF-CRATS  PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-DTRNSF-CRATS-PCELA  PIC  X(010).
                20 DCOMWG5S-DTRNSF-LP-PCELA     PIC  X(010).
                20 DCOMWG5S-CBACEN-DESC-RURAL   PIC S9(009)      COMP-3.
                20 DCOMWG5S-CCONTR-CORP-PCELA   PIC S9(007)      COMP-3.
                20 DCOMWG5S-DULT-ADTTO-PCELA    PIC  X(010).
                20 DCOMWG5S-CINDCD-MOTVO-ADTTO  PIC  X(001).
                20 DCOMWG5S-CINDCD-PEND-EXTER   PIC  X(001).
                20 DCOMWG5S-CINDCD-BLOQ-CONS    PIC  X(001).
                20 DCOMWG5S-DBLOQ-CONS-CCORR    PIC  X(010).
                20 DCOMWG5S-DULT-PRROG-PCELA    PIC  X(010).
                20 DCOMWG5S-DULT-REATV-PCELA    PIC  X(010).
                20 DCOMWG5S-HULT-ATULZ          PIC  X(026).
                20 DCOMWG5S-CFUNC-BDSCO         PIC S9(009)      COMP-3.
                20 DCOMWG5S-CTERM               PIC  X(008).
                20 DCOMWG5S-DINCL-REG           PIC  X(010).
                20 DCOMWG5S-VJURO-REMUN-VENCD   PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VMORA-VENCD-DESC    PIC S9(015)V9(2) COMP-3.
                20 DCOMWG5S-VMULTA-VENCD-DESC   PIC S9(015)V9(2) COMP-3.
             15 DCOMWG5S-FILLER                 PIC  X(2124).
      *                                                                *
      *================================================================*
