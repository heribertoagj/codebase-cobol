      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOX       *
      * ARQUIVO = DETALHE LIBERACAO ISD     SEQUENCIAL                 *
      * LRECL   = 110 BYTES                 23-04-2007                 *
      *----------------------------------------------------------------*
       01 OX-REGISTRO.
 01 04    05 OX-CCTRO-CUSTO                 PIC  X(004).
 05 01    05 OX-FILLER1                     PIC  X(001)  VALUE ';'.
 06 04    05 OX-CEVNTO-ORIGE                PIC  9(004).
 10 01    05 OX-FILLER2                     PIC  X(001)  VALUE ';'.
 11 10    05 OX-DMOVTO                      PIC  X(010).
 21 01    05 OX-FILLER3                     PIC  X(001)  VALUE ';'.
 22 10    05 OX-DEVNTO                      PIC  X(010).
 32 01    05 OX-FILLER4                     PIC  X(001)  VALUE ';'.
 33 03    05 OX-CPRODT                      PIC  9(003).
 36 01    05 OX-FILLER5                     PIC  X(001)  VALUE ';'.
 37 03    05 OX-CSPROD-DESC                 PIC  9(003).
 40 01    05 OX-FILLER6                     PIC  X(001)  VALUE ';'.
 41 03    05 OX-CTPO-DESC                   PIC  9(003).
 44 01    05 OX-FILLER7                     PIC  X(001)  VALUE ';'.
 45 05    05 OX-CCART-NORML                 PIC  X(005).
 50 01    05 OX-FILLER8                     PIC  X(001)  VALUE ';'.
 51 05    05 OX-CAG-OPER-DESC               PIC  9(005).
 56 01    05 OX-FILLER9                     PIC  X(001)  VALUE ';'.
 57 03    05 OX-CMEIO-LIBRC-DESC            PIC  9(003).
 60 01    05 OX-FILLER10                    PIC  X(001)  VALUE ';'.
 61 01    05 OX-SINAL1                      PIC  X(001).
 62 17    05 OX-VTOT-LIQ-LIBRC-CREDT-INT    PIC  9(017).
 79 01    05 OX-FILLER11                    PIC  X(001)  VALUE ','.
 80 02    05 OX-VTOT-LIQ-LIBRC-CREDT-DEC    PIC  9(002).
 82 01    05 OX-FILLER12                    PIC  X(001)  VALUE ';'.
 83 18    05 OX-QTOT-OPER                   PIC  9(018).
101 10    05 FILLER                         PIC  X(010).
