      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRL                                      *
      * TAMANHO : 0316 BYTES                                           *
      * ARQUIVO : DCOMB0A1 - TESTAT_MOVTC_SEGMT                        *
      ******************************************************************
       01  RL-A1-REGISTRO.
           05  RL-A1-DPOSIC-CONSL-DESC   PIC  X(010).
           05  RL-A1-CBCO                PIC S9(003)      COMP-3.
           05  RL-A1-CSGMTO-CLI          PIC S9(003)      COMP-3.
           05  RL-A1-CPRODT              PIC S9(003)      COMP-3.
           05  RL-A1-CSPROD-DESC-COML    PIC S9(003)      COMP-3.
           05  RL-A1-CINDCD-PER-CONSL    PIC  X(001).
           05  RL-A1-CORIGE-OPER-DESC    PIC S9(001)      COMP-3.
           05  RL-A1-CAG-BCRIA           PIC S9(005)      COMP-3.
           05  RL-A1-VSDO-ANTER-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VPOSIC-ENTRD-CONSL  PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VPOSIC-BAIXA-CONSL  PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VTRNSF-MORA-DIA     PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VSDO-ATUAL-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RL-A1-QTITLO-SDO-ANTER    PIC S9(017)      COMP-3.
           05  RL-A1-QENTRD-TITLO-CONSL  PIC S9(017)      COMP-3.
           05  RL-A1-QBAIXA-TITLO-CONSL  PIC S9(017)      COMP-3.
           05  RL-A1-QTITLO-MORA-DIA     PIC S9(017)      COMP-3.
           05  RL-A1-QTITLO-SDO-ATUAL    PIC S9(017)      COMP-3.
           05  RL-A1-QOPER-SDO-ANTER     PIC S9(017)      COMP-3.
           05  RL-A1-QENTRD-OPER-CONSL   PIC S9(017)      COMP-3.
           05  RL-A1-QBAIXA-OPER-CONSL   PIC S9(017)      COMP-3.
           05  RL-A1-QOPER-MORA-DIA      PIC S9(017)      COMP-3.
           05  RL-A1-QOPER-SDO-ATUAL     PIC S9(017)      COMP-3.
           05  RL-A1-VTAC-SDO-ANTER      PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VENTRD-TAC-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VBAIXA-TAC-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VTAC-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VTAC-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RL-A1-PTX-MED-ENTRD       PIC S9(003)V9(5) COMP-3.
           05  RL-A1-PTX-MED-BAIXA       PIC S9(003)V9(5) COMP-3.
           05  RL-A1-PTX-MED-ESTOQ       PIC S9(003)V9(5) COMP-3.
           05  RL-A1-TMED-TITLO-UTILZ    PIC S9(005)      COMP-3.
           05  RL-A1-TMED-TITLO-ESTOQ    PIC S9(005)      COMP-3.
           05  RL-A1-CINDCD-PRZ-MED      PIC  X(002).
           05  RL-A1-VRAP-SDO-ANTER      PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VRAP-ENTRD          PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VRAP-BAIXA          PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VRAP-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VRAP-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VIOF-ENTRD          PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VIOF-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VIOF-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VTARIF-ENTRD        PIC S9(015)V9(2) COMP-3.
           05  RL-A1-VTARIF-SDO-ATUAL    PIC S9(015)V9(2) COMP-3.
