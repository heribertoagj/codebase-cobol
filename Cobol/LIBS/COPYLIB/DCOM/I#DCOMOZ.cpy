      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOZ       *
      * ARQUIVO = BAIXA PARCELA ISD         SEQUENCIAL                 *
      * LRECL   = 400 BYTES                 25-04-2007                 *
      *----------------------------------------------------------------*
       01 OZ-REGISTRO.
 01 04    05 OZ-CCTRO-CUSTO                      PIC  X(004).
 05 01    05 OZ-FILLER1                          PIC  X(001)  VALUE ';'.
 06 04    05 OZ-CEVNOZ-ORIGE                     PIC  9(004).
 10 01    05 OZ-FILLER2                          PIC  X(001)  VALUE ';'.
 11 10    05 OZ-DMOVTO                           PIC  X(010).
 21 01    05 OZ-FILLER3                          PIC  X(001)  VALUE ';'.
 22 10    05 OZ-DEVNTO                           PIC  X(010).
 32 01    05 OZ-FILLER4                          PIC  X(001)  VALUE ';'.
 33 03    05 OZ-CPRODT                           PIC  9(003).
 36 01    05 OZ-FILLER5                          PIC  X(001)  VALUE ';'.
 37 03    05 OZ-CSPROD-DESC                      PIC  9(003).
 40 01    05 OZ-FILLER6                          PIC  X(001)  VALUE ';'.
 41 03    05 OZ-CTPO-DESC                        PIC  9(003).
 44 01    05 OZ-FILLER7                          PIC  X(001)  VALUE ';'.
 45 05    05 OZ-CCART-NORML                      PIC  X(005).
 50 01    05 OZ-FILLER8                          PIC  X(001)  VALUE ';'.
 51 05    05 OZ-CCART-VENCD                      PIC  X(005).
 56 01    05 OZ-FILLER9                          PIC  X(001)  VALUE ';'.
 57 03    05 OZ-CSIT-PCELA-DESC                  PIC  9(003).
 60 01    05 OZ-FILLER10                         PIC  X(001)  VALUE ';'.
 61 05    05 OZ-CAG-OPER-DESC                    PIC  9(005).
 66 01    05 OZ-FILLER11                         PIC  X(001)  VALUE ';'.
 67 01    05 OZ-SINAL1                           PIC  X(001).
 68 17    05 OZ-VTOT-SDO-DVDOR-PCELA-INT         PIC  9(017).
 85 01    05 OZ-FILLER12                         PIC  X(001)  VALUE ','.
 86 02    05 OZ-VTOT-SDO-DVDOR-PCELA-DEC         PIC  9(002).
 88 01    05 OZ-FILLER13                         PIC  X(001)  VALUE ';'.
 89 01    05 OZ-SINAL2                           PIC  X(001).
 90 17    05 OZ-VTOT-PRINC-BAIXA-PCELA-INT       PIC  9(017).
107 01    05 OZ-FILLER14                         PIC  X(001)  VALUE ','.
108 02    05 OZ-VTOT-PRINC-BAIXA-PCELA-DEC       PIC  9(002).
110 01    05 OZ-FILLER15                         PIC  X(001)  VALUE ';'.
111 01    05 OZ-SINAL3                           PIC  X(001).
112 17    05 OZ-VTOT-JURO-BAIXA-PCELA-INT        PIC  9(017).
129 01    05 OZ-FILLER16                         PIC  X(001)  VALUE ','.
130 02    05 OZ-VTOT-JURO-BAIXA-PCELA-DEC        PIC  9(002).
132 01    05 OZ-FILLER17                         PIC  X(001)  VALUE ';'.
133 01    05 OZ-SINAL4                           PIC  X(001).
134 17    05 OZ-VTOT-IOF-BAIXA-PCELA-INT         PIC  9(017).
151 01    05 OZ-FILLER18                         PIC  X(001)  VALUE ','.
152 02    05 OZ-VTOT-IOF-BAIXA-PCELA-DEC         PIC  9(002).
154 01    05 OZ-FILLER19                         PIC  X(001)  VALUE ';'.
155 01    05 OZ-SINAL5                           PIC  X(001).
156 17    05 OZ-VTOT-MULTA-BAIXA-PCELA-INT       PIC  9(017).
173 01    05 OZ-FILLER20                         PIC  X(001)  VALUE ','.
174 02    05 OZ-VTOT-MULTA-BAIXA-PCELA-DEC       PIC  9(002).
176 01    05 OZ-FILLER21                         PIC  X(001)  VALUE ';'.
177 01    05 OZ-SINAL6                           PIC  X(001).
178 17    05 OZ-VTOT-ABTMT-BAIXA-PCELA-INT       PIC  9(017).
195 01    05 OZ-FILLER22                         PIC  X(001)  VALUE ','.
196 02    05 OZ-VTOT-ABTMT-BAIXA-PCELA-DEC       PIC  9(002).
198 01    05 OZ-FILLER23                         PIC  X(001)  VALUE ';'.
199 01    05 OZ-SINAL7                           PIC  X(001).
200 17    05 OZ-VTOT-DESC-BAIXA-PCELA-INT        PIC  9(017).
217 01    05 OZ-FILLER24                         PIC  X(001)  VALUE ','.
218 02    05 OZ-VTOT-DESC-BAIXA-PCELA-DEC        PIC  9(002).
220 01    05 OZ-FILLER25                         PIC  X(001)  VALUE ';'.
221 01    05 OZ-SINAL8                           PIC  X(001).
222 17    05 OZ-VTOT-EFETV-BAIXA-PCELA-INT       PIC  9(017).
239 01    05 OZ-FILLER26                         PIC  X(001)  VALUE ','.
240 02    05 OZ-VTOT-EFETV-BAIXA-PCELA-DEC       PIC  9(002).
242 01    05 OZ-FILLER27                         PIC  X(001)  VALUE ';'.
243 01    05 OZ-SINAL9                           PIC  X(001).
244 17    05 OZ-VTOT-JURO-MORA-LEGAL-INT         PIC  9(017).
261 01    05 OZ-FILLER28                         PIC  X(001)  VALUE ','.
262 02    05 OZ-VTOT-JURO-MORA-LEGAL-DEC         PIC  9(002).
264 01    05 OZ-FILLER29                         PIC  X(001)  VALUE ';'.
265 01    05 OZ-SINAL10                          PIC  X(001).
266 17    05 OZ-VTOT-CUSTO-BAIXA-PCELA-INT       PIC  9(017).
283 01    05 OZ-FILLER30                         PIC  X(001)  VALUE ','.
284 02    05 OZ-VTOT-CUSTO-BAIXA-PCELA-DEC       PIC  9(002).
286 01    05 OZ-FILLER31                         PIC  X(001)  VALUE ';'.
287 01    05 OZ-SINAL11                          PIC  X(001).
288 17    05 OZ-VTOT-JURO-BAIXA-PROVS-INT        PIC  9(017).
305 01    05 OZ-FILLER32                         PIC  X(001)  VALUE ','.
306 02    05 OZ-VTOT-JURO-BAIXA-PROVS-DEC        PIC  9(002).
308 01    05 OZ-FILLER33                         PIC  X(001)  VALUE ';'.
309 01    05 OZ-SINAL12                          PIC  X(001).
310 17    05 OZ-VTOT-CUSTO-BAIXA-PROVS-INT       PIC  9(017).
327 01    05 OZ-FILLER34                         PIC  X(001)  VALUE ','.
328 02    05 OZ-VTOT-CUSTO-BAIXA-PROVS-DEC       PIC  9(002).
330 01    05 OZ-FILLER35                         PIC  X(001)  VALUE ';'.
331 01    05 OZ-SINAL13                          PIC  X(001).
332 17    05 OZ-VTOT-DEVLC-BAIXA-EXCED-INT       PIC  9(017).
349 01    05 OZ-FILLER36                         PIC  X(001)  VALUE ','.
350 02    05 OZ-VTOT-DEVLC-BAIXA-EXCED-DEC       PIC  9(002).
352 01    05 OZ-FILLER37                         PIC  X(001)  VALUE ';'.
353 01    05 OZ-SINAL14                          PIC  X(001).
354 17    05 OZ-VTOT-RECTA-BAIXA-EXCED-INT       PIC  9(017).
371 01    05 OZ-FILLER38                         PIC  X(001)  VALUE ','.
372 02    05 OZ-VTOT-RECTA-BAIXA-EXCED-DEC       PIC  9(002).
374 01    05 OZ-FILLER39                         PIC  X(001)  VALUE ';'.
374 18    05 OZ-QTOT-PCELA                       PIC  9(018).
393 08    05 FILLER                              PIC  X(008).
