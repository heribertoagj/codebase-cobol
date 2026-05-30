      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTE       *
      * ARQUIVO = COMISSAO CONVENIO ISD     SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 23-04-2007                 *
      *----------------------------------------------------------------*
       01 TE-REGISTRO.
 01 04    05 TE-CCTRO-CUSTO                 PIC  X(004).
 05 01    05 TE-FILLER1                     PIC  X(001)  VALUE ';'.
 06 04    05 TE-CEVNTO-ORIGE                PIC  9(004).
 10 01    05 TE-FILLER2                     PIC  X(001)  VALUE ';'.
 11 10    05 TE-DMOVTO                      PIC  X(010).
 21 01    05 TE-FILLER3                     PIC  X(001)  VALUE ';'.
 22 10    05 TE-DEVNTO                      PIC  X(010).
 32 01    05 TE-FILLER4                     PIC  X(001)  VALUE ';'.
 33 03    05 TE-CPRODT                      PIC  9(003).
 36 01    05 TE-FILLER5                     PIC  X(001)  VALUE ';'.
 37 03    05 TE-CSPROD-DESC                 PIC  9(003).
 40 01    05 TE-FILLER6                     PIC  X(001)  VALUE ';'.
 41 02    05 TE-CINDCD-CONTR-CONVE          PIC  X(002).
 43 01    05 TE-FILLER7                     PIC  X(001)  VALUE ';'.
 44 05    05 TE-CAG-OPER-CONTR-CONVE        PIC  9(005).
 49 01    05 TE-FILLER8                     PIC  X(001)  VALUE ';'.
 50 01    05 TE-SINAL1                      PIC  X(001).
 51 17    05 TE-VTOT-CALC-COMIS-CONVE-INT   PIC  9(017).
 68 01    05 TE-FILLER9                     PIC  X(001)  VALUE ','.
 69 02    05 TE-VTOT-CALC-COMIS-CONVE-DEC   PIC  9(002).
 71 01    05 TE-FILLER10                    PIC  X(001)  VALUE ';'.
 72 18    05 TE-QTOT-OPER                   PIC  9(018).
 90 11    05 FILLER                         PIC  X(011).
