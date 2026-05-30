      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTD       *
      * ARQUIVO = SALDO OPERACAO ISD        SEQUENCIAL                 *
      * LRECL   = 560 BYTES                 24-04-2007                 *
      *----------------------------------------------------------------*
       01 TD-REGISTRO.
 01 04    05 TD-CCTRO-CUSTO                      PIC  X(004).
 05 01    05 TD-FILLER1                          PIC  X(001)  VALUE ';'.
 06 04    05 TD-CEVNTO-ORIGE                     PIC  9(004).
 10 01    05 TD-FILLER2                          PIC  X(001)  VALUE ';'.
 11 10    05 TD-DMOVTO                           PIC  X(010).
 21 01    05 TD-FILLER3                          PIC  X(001)  VALUE ';'.
 22 10    05 TD-DEVNTO                           PIC  X(010).
 32 01    05 TD-FILLER4                          PIC  X(001)  VALUE ';'.
 33 03    05 TD-CPRODT                           PIC  9(003).
 36 01    05 TD-FILLER5                          PIC  X(001)  VALUE ';'.
 37 03    05 TD-CSPROD-DESC                      PIC  9(003).
 40 01    05 TD-FILLER6                          PIC  X(001)  VALUE ';'.
 41 03    05 TD-CTPO-DESC                        PIC  9(003).
 44 01    05 TD-FILLER7                          PIC  X(001)  VALUE ';'.
 45 05    05 TD-CCART-NORML                      PIC  X(005).
 50 01    05 TD-FILLER8                          PIC  X(001)  VALUE ';'.
 51 05    05 TD-CCART-VENCD                      PIC  X(005).
 56 01    05 TD-FILLER9                          PIC  X(001)  VALUE ';'.
 57 03    05 TD-CSIT-PCELA-DESC                  PIC  9(003).
 60 01    05 TD-FILLER10                         PIC  X(001)  VALUE ';'.
 61 05    05 TD-CAG-OPER-DESC                    PIC  9(005).
 66 01    05 TD-FILLER11                         PIC  X(001)  VALUE ';'.
 67 01    05 TD-SINAL1                           PIC  X(001).
 68 17    05 TD-VTOT-PSENT-OPER-DESC-INT         PIC  9(017).
 85 01    05 TD-FILLER12                         PIC  X(001)  VALUE ','.
 86 02    05 TD-VTOT-PSENT-OPER-DESC-DEC         PIC  9(002).
 88 01    05 TD-FILLER13                         PIC  X(001)  VALUE ';'.
 89 01    05 TD-SINAL2                           PIC  X(001).
 90 17    05 TD-VTOT-RAP-PSENT-OPER-INT          PIC  9(017).
107 01    05 TD-FILLER14                         PIC  X(001)  VALUE ','.
108 02    05 TD-VTOT-RAP-PSENT-OPER-DEC          PIC  9(002).
110 01    05 TD-FILLER15                         PIC  X(001)  VALUE ';'.
111 01    05 TD-SINAL3                           PIC  X(001).
112 17    05 TD-VTOT-JURO-APROP-ACUM-INT         PIC  9(017).
129 01    05 TD-FILLER16                         PIC  X(001)  VALUE ','.
130 02    05 TD-VTOT-JURO-APROP-ACUM-DEC         PIC  9(002).
132 01    05 TD-FILLER17                         PIC  X(001)  VALUE ';'.
133 01    05 TD-SINAL4                           PIC  X(001).
134 17    05 TD-VTOT-SDO-DVDOR-OPER-INT          PIC  9(017).
151 01    05 TD-FILLER18                         PIC  X(001)  VALUE ','.
152 02    05 TD-VTOT-SDO-DVDOR-OPER-DEC          PIC  9(002).
154 01    05 TD-FILLER19                         PIC  X(001)  VALUE ';'.
155 01    05 TD-SINAL5                           PIC  X(001).
156 17    05 TD-VTOT-SDO-DVDOR-CRATS-INT         PIC  9(017).
173 01    05 TD-FILLER20                         PIC  X(001)  VALUE ','.
174 02    05 TD-VTOT-SDO-DVDOR-CRATS-DEC         PIC  9(002).
176 01    05 TD-FILLER21                         PIC  X(001)  VALUE ';'.
177 01    05 TD-SINAL6                           PIC  X(001).
178 17    05 TD-VTOT-JURO-MORA-LEGAL-INT         PIC  9(017).
195 01    05 TD-FILLER22                         PIC  X(001)  VALUE ','.
196 02    05 TD-VTOT-JURO-MORA-LEGAL-DEC         PIC  9(002).
198 01    05 TD-FILLER23                         PIC  X(001)  VALUE ';'.
199 01    05 TD-SINAL7                           PIC  X(001).
200 17    05 TD-VTOT-JURO-MORA-GERC-INT          PIC  9(017).
217 01    05 TD-FILLER24                         PIC  X(001)  VALUE ','.
218 02    05 TD-VTOT-JURO-MORA-GERC-DEC          PIC  9(002).
220 01    05 TD-FILLER25                         PIC  X(001)  VALUE ';'.
221 01    05 TD-SINAL8                           PIC  X(001).
222 17    05 TD-VTOT-JURO-TRNSF-OPER-INT         PIC  9(017).
239 01    05 TD-FILLER26                         PIC  X(001)  VALUE ','.
240 02    05 TD-VTOT-JURO-TRNSF-OPER-DEC         PIC  9(002).
242 01    05 TD-FILLER27                         PIC  X(001)  VALUE ';'.
243 01    05 TD-SINAL9                           PIC  X(001).
244 17    05 TD-VTOT-JURO-PROVS-OPER-INT         PIC  9(017).
261 01    05 TD-FILLER28                         PIC  X(001)  VALUE ','.
262 02    05 TD-VTOT-JURO-PROVS-OPER-DEC         PIC  9(002).
264 01    05 TD-FILLER29                         PIC  X(001)  VALUE ';'.
265 01    05 TD-SINAL10                          PIC  X(001).
266 17    05 TD-VTOT-DIFCA-COBR-MORA-INT         PIC  9(017).
283 01    05 TD-FILLER30                         PIC  X(001)  VALUE ','.
284 02    05 TD-VTOT-DIFCA-COBR-MORA-DEC         PIC  9(002).
286 01    05 TD-FILLER31                         PIC  X(001)  VALUE ';'.
287 01    05 TD-SINAL11                          PIC  X(001).
288 17    05 TD-VTOT-DIFCA-COBR-CRATS-INT        PIC  9(017).
305 01    05 TD-FILLER32                         PIC  X(001)  VALUE ','.
306 02    05 TD-VTOT-DIFCA-COBR-CRATS-DEC        PIC  9(002).
308 01    05 TD-FILLER33                         PIC  X(001)  VALUE ';'.
309 01    05 TD-SINAL12                          PIC  X(001).
310 17    05 TD-VTOT-CUSTO-OPER-VNCER-INT        PIC  9(017).
327 01    05 TD-FILLER34                         PIC  X(001)  VALUE ','.
328 02    05 TD-VTOT-CUSTO-OPER-VNCER-DEC        PIC  9(002).
330 01    05 TD-FILLER35                         PIC  X(001)  VALUE ';'.
331 01    05 TD-SINAL13                          PIC  X(001).
332 17    05 TD-VTOT-CUSTO-OPER-VENCD-INT        PIC  9(017).
349 01    05 TD-FILLER36                         PIC  X(001)  VALUE ','.
350 02    05 TD-VTOT-CUSTO-OPER-VENCD-DEC        PIC  9(002).
352 01    05 TD-FILLER37                         PIC  X(001)  VALUE ';'.
353 01    05 TD-SINAL14                          PIC  X(001).
354 17    05 TD-VTOT-CUSTO-TRNSF-OPER-INT        PIC  9(017).
371 01    05 TD-FILLER38                         PIC  X(001)  VALUE ','.
372 02    05 TD-VTOT-CUSTO-TRNSF-OPER-DEC        PIC  9(002).
374 01    05 TD-FILLER39                         PIC  X(001)  VALUE ';'.
375 01    05 TD-SINAL15                          PIC  X(001).
376 17    05 TD-VTOT-CUSTO-PROVS-OPER-INT        PIC  9(017).
393 01    05 TD-FILLER40                         PIC  X(001)  VALUE ','.
394 02    05 TD-VTOT-CUSTO-PROVS-OPER-DEC        PIC  9(002).
396 01    05 TD-FILLER41                         PIC  X(001)  VALUE ';'.
397 01    05 TD-SINAL16                          PIC  X(001).
398 17    05 TD-VTOT-JURO-VNCER-OPER-INT         PIC  9(017).
415 01    05 TD-FILLER42                         PIC  X(001)  VALUE ','.
416 02    05 TD-VTOT-JURO-VNCER-OPER-DEC         PIC  9(002).
418 01    05 TD-FILLER43                         PIC  X(001)  VALUE ';'.
419 01    05 TD-SINAL17                          PIC  X(001).
420 17    05 TD-VTOT-JURO-MORA-OPER-INT          PIC  9(017).
437 01    05 TD-FILLER44                         PIC  X(001)  VALUE ','.
438 02    05 TD-VTOT-JURO-MORA-OPER-DEC          PIC  9(002).
440 01    05 TD-FILLER45                         PIC  X(001)  VALUE ';'.
441 01    05 TD-SINAL18                          PIC  X(001).
442 17    05 TD-VTOT-JURO-CRATS-OPER-INT         PIC  9(017).
459 01    05 TD-FILLER46                         PIC  X(001)  VALUE ','.
460 02    05 TD-VTOT-JURO-CRATS-OPER-DEC         PIC  9(002).
462 01    05 TD-FILLER47                         PIC  X(001)  VALUE ';'.
463 01    05 TD-SINAL19                          PIC  X(001).
464 17    05 TD-VTOT-CUSTO-VNCER-OPER-INT        PIC  9(017).
481 01    05 TD-FILLER48                         PIC  X(001)  VALUE ','.
482 02    05 TD-VTOT-CUSTO-VNCER-OPER-DEC        PIC  9(002).
484 01    05 TD-FILLER49                         PIC  X(001)  VALUE ';'.
485 01    05 TD-SINAL20                          PIC  X(001).
486 17    05 TD-VTOT-CUSTO-MORA-OPER-INT         PIC  9(017).
503 01    05 TD-FILLER50                         PIC  X(001)  VALUE ','.
504 02    05 TD-VTOT-CUSTO-MORA-OPER-DEC         PIC  9(002).
506 01    05 TD-FILLER51                         PIC  X(001)  VALUE ';'.
507 01    05 TD-SINAL21                          PIC  X(001).
508 17    05 TD-VTOT-CUSTO-CRATS-OPER-INT        PIC  9(017).
525 01    05 TD-FILLER52                         PIC  X(001)  VALUE ','.
526 02    05 TD-VTOT-CUSTO-CRATS-OPER-DEC        PIC  9(002).
528 01    05 TD-FILLER53                         PIC  X(001)  VALUE ';'.
529 18    05 TD-QTOT-OPER                        PIC  9(018).
547 14    05 FILLER                              PIC  X(014).
