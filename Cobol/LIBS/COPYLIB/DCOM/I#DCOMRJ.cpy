      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRJ                                      *
      * TAMANHO : 0325 BYTES                                           *
      * ARQUIVO : DCOMB099 - TESTAT_MOVTC_CLI                          *
      ******************************************************************
       01  RJ-99-REGISTRO.
           05  RJ-99-DMES-CONSL-PTCAO    PIC S9(002)      COMP-3.
           05  RJ-99-DPOSIC-CONSL-DESC   PIC  X(010).
           05  RJ-99-CBCO                PIC S9(003)      COMP-3.
           05  RJ-99-CAG-BCRIA           PIC S9(005)      COMP-3.
           05  RJ-99-CCTA-BCRIA-CLI      PIC S9(013)      COMP-3.
           05  RJ-99-CPRODT              PIC S9(003)      COMP-3.
           05  RJ-99-CSPROD-DESC-COML    PIC S9(003)      COMP-3.
           05  RJ-99-CINDCD-PER-CONSL    PIC  X(001).
           05  RJ-99-CORIGE-OPER-DESC    PIC S9(001)      COMP-3.
           05  RJ-99-VSDO-ANTER-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VPOSIC-ENTRD-CONSL  PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VPOSIC-BAIXA-CONSL  PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VTRNSF-MORA-DIA     PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VSDO-ATUAL-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RJ-99-QTITLO-SDO-ANTER    PIC S9(017)      COMP-3.
           05  RJ-99-QENTRD-TITLO-CONSL  PIC S9(017)      COMP-3.
           05  RJ-99-QBAIXA-TITLO-CONSL  PIC S9(017)      COMP-3.
           05  RJ-99-QTITLO-MORA-DIA     PIC S9(017)      COMP-3.
           05  RJ-99-QTITLO-SDO-ATUAL    PIC S9(017)      COMP-3.
           05  RJ-99-QOPER-SDO-ANTER     PIC S9(017)      COMP-3.
           05  RJ-99-QENTRD-OPER-CONSL   PIC S9(017)      COMP-3.
           05  RJ-99-QBAIXA-OPER-CONSL   PIC S9(017)      COMP-3.
           05  RJ-99-QOPER-MORA-DIA      PIC S9(017)      COMP-3.
           05  RJ-99-QOPER-SDO-ATUAL     PIC S9(017)      COMP-3.
           05  RJ-99-VTAC-SDO-ANTER      PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VENTRD-TAC-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VBAIXA-TAC-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VTAC-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VTAC-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RJ-99-PTX-MED-ENTRD       PIC S9(003)V9(5) COMP-3.
           05  RJ-99-PTX-MED-BAIXA       PIC S9(003)V9(5) COMP-3.
           05  RJ-99-PTX-MED-ESTOQ       PIC S9(003)V9(5) COMP-3.
           05  RJ-99-TMED-TITLO-UTILZ    PIC S9(005)      COMP-3.
           05  RJ-99-TMED-TITLO-ESTOQ    PIC S9(005)      COMP-3.
           05  RJ-99-CINDCD-PRZ-MED      PIC  X(002).
           05  RJ-99-CSGMTO-CLI          PIC S9(003)      COMP-3.
           05  RJ-99-VRAP-SDO-ANTER      PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VRAP-ENTRD          PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VRAP-BAIXA          PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VRAP-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VRAP-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VIOF-ENTRD          PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VIOF-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VIOF-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VTARIF-ENTRD        PIC S9(015)V9(2) COMP-3.
           05  RJ-99-VTARIF-SDO-ATUAL    PIC S9(015)V9(2) COMP-3.
