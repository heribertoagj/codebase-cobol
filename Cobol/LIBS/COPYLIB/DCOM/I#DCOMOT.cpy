      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOT       *
      * ARQUIVO = OPERACOES DESCONTO ISD    SEQUENCIAL                 *
      * LRECL   = 150 BYTES                 23-04-2007                 *
      *----------------------------------------------------------------*
       01 OT-REGISTRO.
 01 04    05 OT-CCTRO-CUSTO                 PIC  X(004).
 05 01    05 OT-FILLER1                     PIC  X(001)  VALUE ';'.
 06 04    05 OT-CEVNTO-ORIGE                PIC  9(004).
 10 01    05 OT-FILLER2                     PIC  X(001)  VALUE ';'.
 11 10    05 OT-DMOVTO                      PIC  X(010).
 21 01    05 OT-FILLER3                     PIC  X(001)  VALUE ';'.
 22 03    05 OT-CPRODT                      PIC  9(003).
 25 01    05 OT-FILLER4                     PIC  X(001)  VALUE ';'.
 26 03    05 OT-CSPROD-DESC                 PIC  9(003).
 29 01    05 OT-FILLER5                     PIC  X(001)  VALUE ';'.
 30 03    05 OT-CTPO-DESC                   PIC  9(003).
 33 01    05 OT-FILLER6                     PIC  X(001)  VALUE ';'.
 34 05    05 OT-CCART-NORML                 PIC  X(005).
 39 01    05 OT-FILLER7                     PIC  X(001)  VALUE ';'.
 40 05    05 OT-CCART-VENCD                 PIC  X(005).
 45 01    05 OT-FILLER8                     PIC  X(001)  VALUE ';'.
 46 03    05 OT-CSIT-OPER-DESC              PIC  9(003).
 49 01    05 OT-FILLER9                     PIC  X(001)  VALUE ';'.
 50 05    05 OT-CAG-OPER-DESC               PIC  9(005).
 55 01    05 OT-FILLER10                    PIC  X(001)  VALUE ';'.
 56 01    05 OT-CATULZ                      PIC  X(001).
 57 01    05 OT-FILLER11                    PIC  X(001)  VALUE ';'.
 58 02    05 OT-CINDCD-CONTR-CONVE          PIC  X(002).
 60 01    05 OT-FILLER12                    PIC  X(001)  VALUE ';'.
 61 05    05 OT-CAG-OPER-CONTR-CONVE        PIC  9(005).
 66 01    05 OT-FILLER13                    PIC  X(001)  VALUE ';'.
 67 01    05 OT-SINAL1                      PIC  X(001).
 68 17    05 OT-VTOT-OPER-DESC-INT          PIC  9(017).
 85 01    05 OT-FILLER14                    PIC  X(001)  VALUE ','.
 86 02    05 OT-VTOT-OPER-DESC-DEC          PIC  9(002).
 88 01    05 OT-FILLER15                    PIC  X(001)  VALUE ';'.
 89 18    05 OT-QTITLO-CHEQ-OPER            PIC  9(018).
107 01    05 OT-FILLER16                    PIC  X(001)  VALUE ';'.
108 01    05 OT-SINAL2                      PIC  X(001).
109 17    05 OT-VTOT-JURO-OPER-DESC-INT     PIC  9(017).
126 01    05 OT-FILLER17                    PIC  X(001)  VALUE ','.
127 02    05 OT-VTOT-JURO-OPER-DESC-DEC     PIC  9(002).
129 01    05 OT-FILLER18                    PIC  X(001)  VALUE ';'.
130 18    05 OT-QTOT-OPER                   PIC  9(018).
148 03    05 FILLER                         PIC  X(003).
