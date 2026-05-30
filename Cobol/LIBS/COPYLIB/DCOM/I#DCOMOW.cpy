      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOW       *
      * ARQUIVO = LIBERACAO CREDITO ISD     SEQUENCIAL                 *
      * LRECL   = 180 BYTES                 23-04-2007                 *
      *----------------------------------------------------------------*
       01 OW-REGISTRO.
 01 04    05 OW-CCTRO-CUSTO                      PIC  X(004).
 05 01    05 OW-FILLER1                          PIC  X(001)  VALUE ';'.
 06 04    05 OW-CEVNTO-ORIGE                     PIC  9(004).
 10 01    05 OW-FILLER2                          PIC  X(001)  VALUE ';'.
 11 10    05 OW-DMOVTO                           PIC  X(010).
 21 01    05 OW-FILLER3                          PIC  X(001)  VALUE ';'.
 22 10    05 OW-DEVNTO                           PIC  X(010).
 32 01    05 OW-FILLER4                          PIC  X(001)  VALUE ';'.
 33 03    05 OW-CPRODT                           PIC  9(003).
 36 01    05 OW-FILLER5                          PIC  X(001)  VALUE ';'.
 37 03    05 OW-CSPROD-DESC                      PIC  9(003).
 40 01    05 OW-FILLER6                          PIC  X(001)  VALUE ';'.
 41 03    05 OW-CTPO-PROD                        PIC  9(003).
 44 01    05 OW-FILLER7                          PIC  X(001)  VALUE ';'.
 45 05    05 OW-CCART-NORML                      PIC  X(005).
 50 01    05 OW-FILLER8                          PIC  X(001)  VALUE ';'.
 51 03    05 OW-CSIT-OPER-DESC                   PIC  9(003).
 54 01    05 OW-FILLER9                          PIC  X(001)  VALUE ';'.
 55 05    05 OW-CAG-OPER-DESC                    PIC  9(005).
 60 01    05 OW-FILLER10                         PIC  X(001)  VALUE ';'.
 61 01    05 OW-CINDCD-TPO-LIBRC-OPER            PIC  X(001).
 62 01    05 OW-FILLER11                         PIC  X(001)  VALUE ';'.
 63 01    05 OW-SINAL1                           PIC  X(001).
 64 17    05 OW-VTOT-LIBRC-FLUXO-OPER-INT        PIC  9(017).
 81 01    05 OW-FILLER12                         PIC  X(001)  VALUE ','.
 82 02    05 OW-VTOT-LIBRC-FLUXO-OPER-DEC        PIC  9(002).
 84 01    05 OW-FILLER13                         PIC  X(001)  VALUE ';'.
 85 01    05 OW-SINAL2                           PIC  X(001).
 86 17    05 OW-VTOT-LIBRC-AJUST-OPER-INT        PIC  9(017).
103 01    05 OW-FILLER14                         PIC  X(001)  VALUE ','.
104 02    05 OW-VTOT-LIBRC-AJUST-OPER-DEC        PIC  9(002).
106 01    05 OW-FILLER15                         PIC  X(001)  VALUE ';'.
107 01    05 OW-SINAL3                           PIC  X(001).
108 17    05 OW-VTOT-LIQ-LIBRC-DESC-INT          PIC  9(017).
125 01    05 OW-FILLER16                         PIC  X(001)  VALUE ','.
126 02    05 OW-VTOT-LIQ-LIBRC-DESC-DEC          PIC  9(002).
128 01    05 OW-FILLER17                         PIC  X(001)  VALUE ';'.
129 01    05 OW-SINAL4                           PIC  X(001).
130 17    05 OW-VTOT-RENDA-APROP-LIBRC-INT       PIC  9(017).
147 01    05 OW-FILLER18                         PIC  X(001)  VALUE ','.
148 02    05 OW-VTOT-RENDA-APROP-LIBRC-DEC       PIC  9(002).
150 01    05 OW-FILLER19                         PIC  X(001)  VALUE ';'.
151 18    05 OW-QTOT-OPER                        PIC  9(018).
169 12    05 FILLER                              PIC  X(012).
