      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRP                                      *
      * TAMANHO : 0000 BYTES                                           *
      * ARQUIVO : ARQUIVO INTERMEDIARIO PARA CONSULTA CONSOLIDADA      *
      ******************************************************************

       01  RP-REGISTRO.

      *--> CHAVE DESCONTO COMERCIAL
           05  RP-C5-DANO-OPER-DESC       PIC  9(004) COMP-3.
           05  RP-C5-NSEQ-OPER-DESC       PIC  9(009) COMP-3.
           05  RP-C9-NPCELA-DESC-COML     PIC  9(005) COMP-3.
           05  RP-95-CBCO                 PIC  9(003) COMP-3.
           05  RP-95-CAG-BCRIA            PIC  9(005) COMP-3.
           05  RP-95-CCTA-CORR-BCRIA      PIC  9(011) COMP-3.
           05  RP-95-CCHEQ                PIC  9(007) COMP-3.

      *--> CHAVE CONSULTA CONSOLIDADA
           05  RP-C5-CBCO                 PIC  9(003) COMP-3.
           05  RP-C5-CAG-BCRIA            PIC  9(005) COMP-3.
           05  RP-C5-CCTA-BCRIA-CLI       PIC  9(013) COMP-3.
           05  RP-C5-CPRODT               PIC  9(003) COMP-3.
           05  RP-C5-CSPROD-DESC-COML     PIC  9(003) COMP-3.
           05  RP-CORIGE-OPER-DESC        PIC  9(001) COMP-3.
           05  RP-CSGMTO-CLI              PIC  9(003) COMP-3.
           05  RP-CDEPDC                  PIC  9(005) COMP-3.
           05  RP-CGERC-RGNAL             PIC  9(005) COMP-3.
           05  RP-DPOSIC-CONSL-DESC       PIC  X(010).

      *--> DADOS DA OPERACAO
           05  RP-C5-CSIT-DESC-COML       PIC  9(003)      COMP-3.
           05  RP-C5-CBCO-OPER-OPER       PIC  9(003)      COMP-3.
           05  RP-C5-CAG-OPER-OPER        PIC  9(005)      COMP-3.
           05  RP-C5-CTPO-DESC-COML       PIC  9(003)      COMP-3.
           05  RP-C5-DINIC-OPER-DESC      PIC  X(010).
           05  RP-C5-TMED-PONDE-OPER      PIC  9(003)      COMP-3.
           05  RP-C5-PTX-JURO-MES         PIC S9(003)V9(5) COMP-3.
           05  RP-C5-VOPER-DESC-COML      PIC S9(015)V9(2) COMP-3.
           05  RP-C5-VTARIF-REG-TITLO     PIC S9(015)V9(2) COMP-3.
           05  RP-C5-VTAC-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           05  RP-C5-VIOF-OPER-DESC       PIC S9(015)V9(2) COMP-3.
           05  RP-TIPO-MOVTO-OPER         PIC  X(003).
           05  FILLER                     PIC  X(010).

      *--> DADOS DA PARCELA
           05  RP-C9-CSIT-DESC-COML       PIC  9(003)      COMP-3.
           05  RP-C9-DVCTO-PCELA-DESC     PIC  X(010).
           05  RP-C9-VPRINC-PCELA-DESC    PIC S9(015)V9(2) COMP-3.
           05  RP-C9-QTITLO-CHEQ-PCELA    PIC S9(009)      COMP-3.
           05  RP-C9-VJURO-PCELA-DESC     PIC S9(015)V9(2) COMP-3.
           05  RP-C9-VIOF-PCELA-DESC      PIC S9(015)V9(2) COMP-3.
           05  RP-C9-VPSENT-PCELA-DESC    PIC S9(015)V9(2) COMP-3.
           05  RP-C9-VJURO-APROP-ACUM     PIC S9(015)V9(2) COMP-3.
           05  RP-C9-DULT-APROP-PCELA     PIC  X(010).
           05  RP-C9-DULT-ADTTO-PCELA     PIC  X(010).
           05  RP-C9-DULT-PRROG-PCELA     PIC  X(010).
           05  RP-C9-DULT-REATV-PCELA     PIC  X(010).
           05  FILLER                     PIC  X(020).

      *--> DADOS DO CHEQUE
           05  RP-95-VCHEQC               PIC S9(013)V9(2) COMP-3.
           05  RP-95-DMODIF-CHEQC-PCELA   PIC  X(010).
           05  RP-95-CINDCD-MOTVO-MODIF   PIC  X(001).
           05  RP-95-VSDO-DVDOR-PRINC     PIC S9(015)V9(2) COMP-3.
           05  RP-95-VJURO-MORA-LEGAL     PIC S9(015)V9(2) COMP-3.
           05  RP-95-VIOF-PCELA-VENCD     PIC S9(015)V9(2) COMP-3.
           05  RP-95-VJURO-TRNSF-CRATS    PIC S9(015)V9(2) COMP-3.
           05  FILLER                     PIC  X(010).

      *--> DADOS DA APROPRIACAO (PARCELA E/OU CHEQUE)
           05  RP-NB-VLR-IOF-VENC         PIC S9(015)V9(2) COMP-3.
           05  RP-NB-VLR-JRS-MORA-LEG     PIC S9(015)V9(2) COMP-3.
           05  RP-NB-VLR-SDO-DEVPRINC     PIC S9(015)V9(2) COMP-3.
           05  RP-NB-DTA-TRF-CRATS        PIC  X(010).
           05  RP-NB-VLR-JRS-TRF-CRATS    PIC S9(015)V9(2) COMP-3.
           05  RP-NB-VLR-PRES-ANT         PIC S9(015)V9(2) COMP-3.
           05  RP-NB-VLR-IOF-VENC-ANT     PIC S9(015)V9(2) COMP-3.
           05  RP-NB-VLR-JRS-MORA-LEG-ANT PIC S9(015)V9(2) COMP-3.
           05  RP-NB-VLR-SDO-DEVPRINC-ANT PIC S9(015)V9(2) COMP-3.
           05  FILLER                     PIC  X(020).

      *--> OUTROS DADOS
           05  RP-C9-CSIT-DESC-COML-ANT   PIC  9(003)      COMP-3.
           05  RP-94-DEFETV-BAIXA-PCELA   PIC  X(010).
           05  RP-94-VPRINC-BAIXA-PCELA   PIC S9(015)V9(2) COMP-3.
           05  RP-94-VMORA-LEGAL-BAIXA    PIC S9(015)V9(2) COMP-3.
           05  RP-DBAIXA-OPER-DESC        PIC  X(010).
           05  RP-DTRAG-TRANSF-ADIT       PIC  X(010).
           05  RP-REG-AGENC-ANT-TRAG      PIC  X(001).
           05  RP-VJURO-ADIT-PRROG        PIC S9(015)V9(2) COMP-3.
           05  RP-VIOF-ADIT-PRROG         PIC S9(015)V9(2) COMP-3.
           05  RP-VTAC-ADIT-PRROG         PIC S9(015)V9(2) COMP-3.
           05  RP-CFAIXA-PRZ-CONSL        PIC  9(003)      COMP-3.
           05  FILLER                     PIC  X(011).

      *--> DADOS PARA CONSOLIDACAO DIARIA (ESTATISTICAS)
           05  RP-VPOSIC-ENTRD-CONSL      PIC S9(015)V9(2) COMP-3.
           05  RP-VPOSIC-BAIXA-CONSL      PIC S9(015)V9(2) COMP-3.
           05  RP-VTRNSF-MORA-DIA         PIC S9(015)V9(2) COMP-3.
           05  RP-QENTRD-TITLO-CONSL      PIC S9(017)      COMP-3.
           05  RP-QBAIXA-TITLO-CONSL      PIC S9(017)      COMP-3.
           05  RP-QTITLO-MORA-DIA         PIC S9(017)      COMP-3.
           05  RP-QENTRD-OPER-CONSL       PIC S9(017)      COMP-3.
           05  RP-QBAIXA-OPER-CONSL       PIC S9(017)      COMP-3.
           05  RP-QOPER-MORA-DIA          PIC S9(017)      COMP-3.
           05  RP-VENTRD-TAC-CONSL        PIC S9(015)V9(2) COMP-3.
           05  RP-VBAIXA-TAC-CONSL        PIC S9(015)V9(2) COMP-3.
           05  RP-VTAC-MORA-DIA           PIC S9(015)V9(2) COMP-3.
           05  RP-PTX-MED-ENTRD           PIC S9(003)V9(5) COMP-3.
           05  RP-PTX-MED-BAIXA           PIC S9(003)V9(5) COMP-3.
           05  RP-PTX-MED-ESTOQ           PIC S9(003)V9(5) COMP-3.
           05  RP-TMED-TITLO-UTILZ        PIC S9(005)      COMP-3.
           05  RP-TMED-TITLO-ESTOQ        PIC S9(005)      COMP-3.
           05  RP-VRAP-ENTRD              PIC S9(015)V9(2) COMP-3.
           05  RP-VRAP-BAIXA              PIC S9(015)V9(2) COMP-3.
           05  RP-VRAP-MORA-DIA           PIC S9(015)V9(2) COMP-3.
           05  RP-VIOF-ENTRD              PIC S9(015)V9(2) COMP-3.
           05  RP-VIOF-MORA-DIA           PIC S9(015)V9(2) COMP-3.
           05  RP-VTARIF-ENTRD            PIC S9(015)V9(2) COMP-3.

      *--> DADOS PARA CONSOLIDACAO DIARIA (PROVISAO / INADIMPLENCIA)
           05  RP-VPRINC-PCELA-CONSL      PIC S9(015)V9(2) COMP-3.
           05  RP-VENCAR-PCELA-CONSL      PIC S9(015)V9(2) COMP-3.
           05  RP-QTITLO-POSIC-CONSL      PIC S9(015)      COMP-3.
           05  RP-VPSENT-PCELA-DESC       PIC S9(015)V9(2) COMP-3.
           05  RP-VRAP-SDO-ATUAL          PIC S9(015)V9(2) COMP-3.

      *--> DADOS PARA MONTAGEM DO SALDO ANTERIOR/ESTOQUE (ESTATISTICAS)
           05  RP-VSDO-ANTER-CONSL        PIC S9(015)V9(2) COMP-3.
           05  RP-VRAP-SDO-ANTER          PIC S9(015)V9(2) COMP-3.
           05  RP-QOPER-SDO-ANTER         PIC S9(017)      COMP-3.
           05  RP-QTITLO-SDO-ANTER        PIC S9(017)      COMP-3.

      *--> DADOS PARA CONSOLIDACAO MENSAL
           05  RP-DMES-CONSL              PIC  9(002).
           05  RP-DULT-MOVTO-CONSL        PIC  X(010).
           05  RP-VTOT-VOPER-ENTD         PIC S9(015)V9(2) COMP-3.
           05  RP-VTOT-VOPER-BXAS         PIC S9(015)V9(2) COMP-3.
           05  RP-VTOT-VOPER-ESTQ         PIC S9(015)V9(2) COMP-3.
           05  RP-VTOT-PTX-JURO-ENTD      PIC S9(011)V9(5) COMP-3.
           05  RP-VTOT-PTX-JURO-BXAS      PIC S9(011)V9(5) COMP-3.
           05  RP-VTOT-PTX-JURO-ESTQ      PIC S9(011)V9(5) COMP-3.
           05  RP-QTOT-TMED-PONDE-ENTD    PIC  9(013)      COMP-3.
           05  RP-QTOT-TMED-PONDE-ESTQ    PIC  9(013)      COMP-3.
           05  FILLER                     PIC  X(010).

           05  FILLER                     PIC  X(031).
