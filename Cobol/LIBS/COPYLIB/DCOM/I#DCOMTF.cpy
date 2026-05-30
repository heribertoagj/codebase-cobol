      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTF       *
      * ARQUIVO = TARIFA CONTRATO ISD       SEQUENCIAL                 *
      * LRECL   = 115 BYTES                 23-04-2007                 *
      *----------------------------------------------------------------*
       01 TF-REGISTRO.
 01 04    05 TF-CCTRO-CUSTO                      PIC  X(004).
 05 01    05 TF-FILLER1                          PIC  X(001)  VALUE ';'.
 06 04    05 TF-CEVNTO-ORIGE                     PIC  9(004).
 10 01    05 TF-FILLER2                          PIC  X(001)  VALUE ';'.
 11 10    05 TF-DMOVTO                           PIC  X(010).
 21 01    05 TF-FILLER3                          PIC  X(001)  VALUE ';'.
 22 10    05 TF-DEVNTO                           PIC  X(010).
 32 01    05 TF-FILLER4                          PIC  X(001)  VALUE ';'.
 33 03    05 TF-CPRODT                           PIC  9(003).
 36 01    05 TF-FILLER5                          PIC  X(001)  VALUE ';'.
 37 03    05 TF-CSPROD-DESC                      PIC  9(003).
 40 01    05 TF-FILLER6                          PIC  X(001)  VALUE ';'.
 41 02    05 TF-CINDICD-CONTR-CONVE              PIC  X(002).
 43 01    05 TF-FILLER7                          PIC  X(001)  VALUE ';'.
 44 05    05 TF-CAG-BCRIA-CONTR-CONVE            PIC  9(005).
 49 01    05 TF-FILLER8                          PIC  X(001)  VALUE ';'.
 50 01    05 TF-SINAL1                           PIC  X(001).
 51 17    05 TF-VTOT-TAC-CONTR-LIM-INT           PIC  9(017).
 68 01    05 TF-FILLER9                          PIC  X(001)  VALUE ','.
 69 02    05 TF-VTOT-TAC-CONTR-LIM-DEC           PIC  9(002).
 71 01    05 TF-FILLER10                         PIC  X(001)  VALUE ';'.
 72 01    05 TF-SINAL2                           PIC  X(001).
 73 17    05 TF-VTOT-TARIF-ADTTO-CONTR-INT       PIC  9(017).
 90 01    05 TF-FILLER11                         PIC  X(001)  VALUE ','.
 91 02    05 TF-VTOT-TARIF-ADTTO-CONTR-DEC       PIC  9(002).
 93 01    05 TF-FILLER12                         PIC  X(001)  VALUE ';'.
 94 18    05 TF-QTOT-OPER                        PIC  9(018).
112 04    05 FILLER                              PIC  X(004).
