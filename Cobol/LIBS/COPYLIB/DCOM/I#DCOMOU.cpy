      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOU       *
      * ARQUIVO = TARIFA OPER DESC ISD      SEQUENCIAL                 *
      * LRECL   = 140 BYTES                 23-04-2007                 *
      *----------------------------------------------------------------*
       01 OU-REGISTRO.
 01 04    05 OU-CCTRO-CUSTO                      PIC  X(004).
 05 01    05 OU-FILLER1                          PIC  X(001)  VALUE ';'.
 06 04    05 OU-CEVNTO-ORIGE                     PIC  9(004).
 10 01    05 OU-FILLER2                          PIC  X(001)  VALUE ';'.
 11 10    05 OU-DMOVTO                           PIC  X(010).
 21 01    05 OU-FILLER3                          PIC  X(001)  VALUE ';'.
 22 10    05 OU-DEVNTO                           PIC  X(010).
 32 01    05 OU-FILLER4                          PIC  X(001)  VALUE ';'.
 33 03    05 OU-CPRODT                           PIC  9(003).
 36 01    05 OU-FILLER5                          PIC  X(001)  VALUE ';'.
 37 03    05 OU-CSPROD-DESC                      PIC  9(003).
 40 01    05 OU-FILLER6                          PIC  X(001)  VALUE ';'.
 41 03    05 OU-CTPO-DESC                        PIC  9(003).
 44 01    05 OU-FILLER7                          PIC  X(001)  VALUE ';'.
 45 05    05 OU-CCART-NORML                      PIC  X(005).
 50 01    05 OU-FILLER8                          PIC  X(001)  VALUE ';'.
 51 05    05 OU-CCART-VENCD                      PIC  X(005).
 56 01    05 OU-FILLER9                          PIC  X(001)  VALUE ';'.
 57 03    05 OU-CSIT-OPER-DESC                   PIC  9(003).
 60 01    05 OU-FILLER10                         PIC  X(001)  VALUE ';'.
 61 05    05 OU-CAG-OPER-DESC                    PIC  9(005).
 66 01    05 OU-FILLER11                         PIC  X(001)  VALUE ';'.
 67 01    05 OU-SINAL1                           PIC  X(001).
 68 17    05 OU-VTOT-TAC-OPER-DESC-INT           PIC  9(017).
 85 01    05 OU-FILLER12                         PIC  X(001)  VALUE ','.
 86 02    05 OU-VTOT-TAC-OPER-DESC-DEC           PIC  9(002).
 88 01    05 OU-FILLER13                         PIC  X(001)  VALUE ';'.
 89 01    05 OU-SINAL2                           PIC  X(001).
 90 17    05 OU-VTOT-TARIF-TITLO-DESC-INT        PIC  9(017).
107 01    05 OU-FILLER14                         PIC  X(001)  VALUE ','.
108 02    05 OU-VTOT-TARIF-TITLO-DESC-DEC        PIC  9(002).
110 01    05 OU-FILLER15                         PIC  X(001)  VALUE ';'.
111 18    05 OU-QTOT-OPER                        PIC  9(018).
129 12    05 FILLER                              PIC  X(012).
