      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOV       *
      * ARQUIVO = TRIBUTO OPER DESC ISD     SEQUENCIAL                 *
      * LRECL   = 115 BYTES                 23-04-2007                 *
      *----------------------------------------------------------------*
       01 OV-REGISTRO.
 01 04    05 OV-CCTRO-CUSTO                 PIC  X(004).
 05 01    05 OV-FILLER1                     PIC  X(001)  VALUE ';'.
 06 04    05 OV-CEVNTO-ORIGE                PIC  9(004).
 10 01    05 OV-FILLER2                     PIC  X(001)  VALUE ';'.
 11 10    05 OV-DMOVTO                      PIC  X(010).
 21 01    05 OV-FILLER3                     PIC  X(001)  VALUE ';'.
 22 10    05 OV-DEVNTO                      PIC  X(010).
 32 01    05 OV-FILLER4                     PIC  X(001)  VALUE ';'.
 33 03    05 OV-CPRODT                      PIC  9(003).
 36 01    05 OV-FILLER5                     PIC  X(001)  VALUE ';'.
 37 03    05 OV-CSPROD-DESC                 PIC  9(003).
 40 01    05 OV-FILLER6                     PIC  X(001)  VALUE ';'.
 41 03    05 OV-CTPO-DESC                   PIC  9(003).
 44 01    05 OV-FILLER7                     PIC  X(001)  VALUE ';'.
 45 05    05 OV-CCART-NORML                 PIC  X(005).
 50 01    05 OV-FILLER8                     PIC  X(001)  VALUE ';'.
 51 05    05 OV-CCART-VENCD                 PIC  X(005).
 56 01    05 OV-FILLER9                     PIC  X(001)  VALUE ';'.
 57 03    05 OV-CSIT-OPER-DESC              PIC  9(003).
 60 01    05 OV-FILLER10                    PIC  X(001)  VALUE ';'.
 61 05    05 OV-CAG-OPER-DESC               PIC  9(005).
 66 01    05 OV-FILLER11                    PIC  X(001)  VALUE ';'.
 67 01    05 OV-SINAL1                      PIC  X(001).
 68 17    05 OV-VTOT-IOF-OPER-DESC-INT      PIC  9(017).
 85 01    05 OV-FILLER12                    PIC  X(001)  VALUE ','.
 86 02    05 OV-VTOT-IOF-OPER-DESC-DEC      PIC  9(002).
 88 01    05 OV-FILLER13                    PIC  X(001)  VALUE ';'.
 89 18    05 OV-QTOT-OPER                   PIC  9(018).
107 09    05 FILLER                         PIC  X(009).
