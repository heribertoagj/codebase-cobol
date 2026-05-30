      ******************************************************************
      *SISTEMA = NET EMPRESA - XXXXXXXX                BOOK = I#DCOMU5 *
      *ARQUIVO = PARCELAS SUMARIZADO                                   *
      *LRECL   = 200 BYTES                             07/08/2007      *
      ******************************************************************
001    01 U5-REGISTRO.
001       05 U5-CHAVE.
001          10 U5-CCORR.
001 02          15 U5-CBCO             PIC  S9(003)V     COMP-3.
003 03          15 U5-CAG-BCRIA        PIC  S9(005)V     COMP-3.
006 07          15 U5-CCTA-BCRIA-CLI   PIC  S9(013)V     COMP-3.
013 02       10 U5-CPRODT              PIC  S9(003)V     COMP-3.
015 02       10 U5-CSPROD-DESC-COML    PIC  S9(003)V     COMP-3.
017 08       10 U5-DPOSIC-CONSL-DESC   PIC   9(008).
017          10 FILLER  REDEFINES      U5-DPOSIC-CONSL-DESC.
017 04          15 U5-DANODPOSIC       PIC   9(004).
021 02          15 U5-DMESDPOSIC       PIC   9(002).
023 02          15 U5-DDIADPOSIC       PIC   9(002).
025       05 U5-AVENC-DBASE.
025 09       10 U5-VPOSIC-AVENC-DBASE  PIC  S9(015)V99   COMP-3.
034 09       10 U5-QTITLO-AVENC-DBASE  PIC  S9(017)V     COMP-3.
043       05 U5-AVENC-TRNTA-DIA.
043 09       10 U5-VPOSIC-AVENC-TRNTA-DIA
                                       PIC  S9(015)V99   COMP-3.
052 09       10 U5-QTITLO-AVENC-TRNTA-DIA
                                       PIC  S9(017)V     COMP-3.
061       05 U5-AVENC-SSSTA-DIA.
061 09       10 U5-VPOSIC-AVENC-SSSTA-DIA
                                       PIC  S9(015)V99   COMP-3.
070 09       10 U5-QTITLO-AVENC-SSSTA-DIA
                                       PIC  S9(017)V     COMP-3.
079       05 U5-AVENC-MSSSTA-DIA.
079 09       10 U5-VPOSIC-AVENC-MSSSTA-DIA
                                       PIC  S9(015)V99   COMP-3.
088 09       10 U5-QTITLO-AVENC-MSSSTA-DIA
                                       PIC  S9(017)V     COMP-3.
097       05 U5-VENCD-DBASE.
097 09       10 U5-VPOSIC-VENCD-DTBAS  PIC  S9(015)V99   COMP-3.
106 09       10 U5-QTITLO-VENCD-DTBAS  PIC  S9(017)V     COMP-3.
115       05 U5-VENCD-TRNTA-DIA.
115 09       10 U5-VPOSIC-VENCD-TRNTA-DIA
                                       PIC  S9(015)V99   COMP-3.
124 09       10 U5-QTITLO-VENCD-TRNTA-DIA
                                       PIC  S9(017)V     COMP-3.
133       05 U5-VENCD-SSSTA-DIA.
133 09       10 U5-VPOSIC-VENCD-SSSTA-DIA
                                       PIC  S9(015)V99   COMP-3.
142 09       10 U5-QTITLO-VENCD-SSSTA-DIA
                                       PIC  S9(017)V     COMP-3.
151       05 U5-VENCD-MSSSTA-DIA.
151 09       10 U5-VPOSIC-VENCD-MSSSTA-DIA
                                       PIC  S9(015)V99   COMP-3.
160 09       10 U5-QTITLO-VENCD-MSSSTA-DIA
                                       PIC  S9(017)V     COMP-3.
169 32    05 FILLER                    PIC   X(032).
