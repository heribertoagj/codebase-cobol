      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRK                                      *
      * TAMANHO : 0321 BYTES                                           *
      * ARQUIVO : DCOMB0A0 - TESTAT_MOVTC_DEPDC                        *
      ******************************************************************
       01  RK-A0-REGISTRO.
           05  RK-A0-DPOSIC-CONSL-DESC   PIC  X(010).
           05  RK-A0-CEMPR-INC           PIC S9(005)      COMP-3.
           05  RK-A0-CDEPDC              PIC S9(005)      COMP-3.
           05  RK-A0-CGERC-RGNAL         PIC S9(005)      COMP-3.
           05  RK-A0-CAG-BCRIA           PIC S9(005)      COMP-3.
           05  RK-A0-CPRODT              PIC S9(003)      COMP-3.
           05  RK-A0-CSPROD-DESC-COML    PIC S9(003)      COMP-3.
           05  RK-A0-CINDCD-PER-CONSL    PIC  X(001).
           05  RK-A0-CORIGE-OPER-DESC    PIC S9(001)      COMP-3.
           05  RK-A0-VSDO-ANTER-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VPOSIC-ENTRD-CONSL  PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VPOSIC-BAIXA-CONSL  PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VTRNSF-MORA-DIA     PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VSDO-ATUAL-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RK-A0-QTITLO-SDO-ANTER    PIC S9(017)      COMP-3.
           05  RK-A0-QENTRD-TITLO-CONSL  PIC S9(017)      COMP-3.
           05  RK-A0-QBAIXA-TITLO-CONSL  PIC S9(017)      COMP-3.
           05  RK-A0-QTITLO-MORA-DIA     PIC S9(017)      COMP-3.
           05  RK-A0-QTITLO-SDO-ATUAL    PIC S9(017)      COMP-3.
           05  RK-A0-QOPER-SDO-ANTER     PIC S9(017)      COMP-3.
           05  RK-A0-QENTRD-OPER-CONSL   PIC S9(017)      COMP-3.
           05  RK-A0-QBAIXA-OPER-CONSL   PIC S9(017)      COMP-3.
           05  RK-A0-QOPER-MORA-DIA      PIC S9(017)      COMP-3.
           05  RK-A0-QOPER-SDO-ATUAL     PIC S9(017)      COMP-3.
           05  RK-A0-VTAC-SDO-ANTER      PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VENTRD-TAC-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VBAIXA-TAC-CONSL    PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VTAC-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VTAC-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RK-A0-PTX-MED-ENTRD       PIC S9(003)V9(5) COMP-3.
           05  RK-A0-PTX-MED-BAIXA       PIC S9(003)V9(5) COMP-3.
           05  RK-A0-PTX-MED-ESTOQ       PIC S9(003)V9(5) COMP-3.
           05  RK-A0-TMED-TITLO-UTILZ    PIC S9(005)      COMP-3.
           05  RK-A0-TMED-TITLO-ESTOQ    PIC S9(005)      COMP-3.
           05  RK-A0-CINDCD-PRZ-MED      PIC  X(002).
           05  RK-A0-VRAP-SDO-ANTER      PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VRAP-ENTRD          PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VRAP-BAIXA          PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VRAP-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VRAP-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VIOF-ENTRD          PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VIOF-MORA-DIA       PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VIOF-SDO-ATUAL      PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VTARIF-ENTRD        PIC S9(015)V9(2) COMP-3.
           05  RK-A0-VTARIF-SDO-ATUAL    PIC S9(015)V9(2) COMP-3.
