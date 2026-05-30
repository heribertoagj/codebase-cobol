      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOY       *
      * ARQUIVO = PARCELA OPERACAO ISD      SEQUENCIAL                 *
      * LRECL   = 730 BYTES                 24-04-2007                 *
      *----------------------------------------------------------------*
       01 OY-REGISTRO.
 01 04    05 OY-CCTRO-CUSTO                 PIC  X(004).
 05 01    05 OY-FILLER1                     PIC  X(001)  VALUE ';'.
 06 04    05 OY-CEVNOY-ORIGE                PIC  9(004).
 10 01    05 OY-FILLER2                     PIC  X(001)  VALUE ';'.
 11 10    05 OY-DMOVTO                      PIC  X(010).
 21 01    05 OY-FILLER3                     PIC  X(001)  VALUE ';'.
 22 10    05 OY-DEVNTO                      PIC  X(010).
 32 01    05 OY-FILLER4                     PIC  X(001)  VALUE ';'.
 33 03    05 OY-CPRODT                      PIC  9(003).
 36 01    05 OY-FILLER5                     PIC  X(001)  VALUE ';'.
 37 03    05 OY-CSPROD-DESC                 PIC  9(003).
 40 01    05 OY-FILLER6                     PIC  X(001)  VALUE ';'.
 41 03    05 OY-CTPO-DESC                   PIC  9(003).
 44 01    05 OY-FILLER7                     PIC  X(001)  VALUE ';'.
 45 05    05 OY-CCART-NORML                 PIC  X(005).
 50 01    05 OY-FILLER8                     PIC  X(001)  VALUE ';'.
 51 05    05 OY-CCART-VENCD                 PIC  X(005).
 56 01    05 OY-FILLER9                     PIC  X(001)  VALUE ';'.
 57 03    05 OY-CSIT-PCELA-DESC             PIC  9(003).
 60 01    05 OY-FILLER10                    PIC  X(001)  VALUE ';'.
 61 05    05 OY-CAG-OPER-DESC               PIC  9(005).
 66 01    05 OY-FILLER11                    PIC  X(001)  VALUE ';'.
 67 18    05 OY-QTOT-TITLO-CHEQ-PCELA       PIC  9(018).
 85 01    05 OY-FILLER12                    PIC  X(001)  VALUE ';'.
 86 01    05 OY-SINAL1                      PIC  X(001).
 87 17    05 OY-VTOT-TARIF-PCELA-INT        PIC  9(017).
104 01    05 OY-FILLER13                    PIC  X(001)  VALUE ','.
105 02    05 OY-VTOT-TARIF-PCELA-DEC        PIC  9(002).
107 01    05 OY-FILLER14                    PIC  X(001)  VALUE ';'.
108 01    05 OY-SINAL2                      PIC  X(001).
109 17    05 OY-VTOT-PRINC-PCELA-DESC-INT   PIC  9(017).
126 01    05 OY-FILLER15                    PIC  X(001)  VALUE ','.
127 02    05 OY-VTOT-PRINC-PCELA-DESC-DEC   PIC  9(002).
129 01    05 OY-FILLER16                    PIC  X(001)  VALUE ';'.
130 01    05 OY-SINAL3                      PIC  X(001).
131 17    05 OY-VTOT-IOF-PCELA-DESC-INT     PIC  9(017).
148 01    05 OY-FILLER17                    PIC  X(001)  VALUE ','.
149 02    05 OY-VTOT-IOF-PCELA-DESC-DEC     PIC  9(002).
151 01    05 OY-FILLER18                    PIC  X(001)  VALUE ';'.
152 01    05 OY-SINAL4                      PIC  X(001).
153 17    05 OY-VTOT-JURO-PCELA-DESC-INT    PIC  9(017).
170 01    05 OY-FILLER19                    PIC  X(001)  VALUE ','.
171 02    05 OY-VTOT-JURO-PCELA-DESC-DEC    PIC  9(002).
173 01    05 OY-FILLER20                    PIC  X(001)  VALUE ';'.
174 01    05 OY-SINAL5                      PIC  X(001).
175 17    05 OY-VTOT-JURO-DIA-PCELA-INT     PIC  9(017).
192 01    05 OY-FILLER21                    PIC  X(001)  VALUE ','.
193 02    05 OY-VTOT-JURO-DIA-PCELA-DEC     PIC  9(002).
195 01    05 OY-FILLER22                    PIC  X(001)  VALUE ';'.
196 01    05 OY-SINAL6                      PIC  X(001).
197 17    05 OY-VTOT-JURO-DATA-EVNTO-INT    PIC  9(017).
214 01    05 OY-FILLER23                    PIC  X(001)  VALUE ','.
215 02    05 OY-VTOT-JURO-DATA-EVNTO-DEC    PIC  9(002).
217 01    05 OY-FILLER24                    PIC  X(001)  VALUE ';'.
218 01    05 OY-SINAL7                      PIC  X(001).
219 17    05 OY-VTOT-JURO-APROP-ACUM-INT    PIC  9(017).
236 01    05 OY-FILLER25                    PIC  X(001)  VALUE ','.
237 02    05 OY-VTOT-JURO-APROP-ACUM-DEC    PIC  9(002).
239 01    05 OY-FILLER26                    PIC  X(001)  VALUE ';'.
240 01    05 OY-SINAL8                      PIC  X(001).
241 17    05 OY-VTOT-PSENT-PCELA-DESC-INT   PIC  9(017).
258 01    05 OY-FILLER27                    PIC  X(001)  VALUE ','.
259 02    05 OY-VTOT-PSENT-PCELA-DESC-DEC   PIC  9(002).
261 01    05 OY-FILLER28                    PIC  X(001)  VALUE ';'.
262 01    05 OY-SINAL9                      PIC  X(001).
263 17    05 OY-VTOT-RAP-PSENT-PCELA-INT    PIC  9(017).
280 01    05 OY-FILLER29                    PIC  X(001)  VALUE ','.
281 02    05 OY-VTOT-RAP-PSENT-PCELA-DEC    PIC  9(002).
283 01    05 OY-FILLER30                    PIC  X(001)  VALUE ';'.
284 01    05 OY-SINAL10                     PIC  X(001).
285 17    05 OY-VTOT-SDO-DVDOR-PRINC-INT    PIC  9(017).
302 01    05 OY-FILLER31                    PIC  X(001)  VALUE ','.
303 02    05 OY-VTOT-SDO-DVDOR-PRINC-DEC    PIC  9(002).
305 01    05 OY-FILLER32                    PIC  X(001)  VALUE ';'.
306 01    05 OY-SINAL11                     PIC  X(001).
307 17    05 OY-VTOT-IOF-PCELA-VENCD-INT    PIC  9(017).
324 01    05 OY-FILLER33                    PIC  X(001)  VALUE ','.
325 02    05 OY-VTOT-IOF-PCELA-VENCD-DEC    PIC  9(002).
327 01    05 OY-FILLER34                    PIC  X(001)  VALUE ';'.
328 01    05 OY-SINAL12                     PIC  X(001).
329 17    05 OY-VTOT-JURO-MORA-LEGAL-INT    PIC  9(017).
346 01    05 OY-FILLER35                    PIC  X(001)  VALUE ','.
347 02    05 OY-VTOT-JURO-MORA-LEGAL-DEC    PIC  9(002).
349 01    05 OY-FILLER36                    PIC  X(001)  VALUE ';'.
350 01    05 OY-SINAL13                     PIC  X(001).
351 17    05 OY-VTOT-JURO-MORA-GERC-INT     PIC  9(017).
368 01    05 OY-FILLER37                    PIC  X(001)  VALUE ','.
369 02    05 OY-VTOT-JURO-MORA-GERC-DEC     PIC  9(002).
371 01    05 OY-FILLER38                    PIC  X(001)  VALUE ';'.
372 01    05 OY-SINAL14                     PIC  X(001).
373 17    05 OY-VTOT-JURO-TRNSF-CRATS-INT   PIC  9(017).
390 01    05 OY-FILLER39                    PIC  X(001)  VALUE ','.
391 02    05 OY-VTOT-JURO-TRNSF-CRATS-DEC   PIC  9(002).
393 01    05 OY-FILLER40                    PIC  X(001)  VALUE ';'.
394 01    05 OY-SINAL15                     PIC  X(001).
395 17    05 OY-VTOT-JURO-PROVS-CRATS-INT   PIC  9(017).
412 01    05 OY-FILLER41                    PIC  X(001)  VALUE ','.
413 02    05 OY-VTOT-JURO-PROVS-CRATS-DEC   PIC  9(002).
415 01    05 OY-FILLER42                    PIC  X(001)  VALUE ';'.
416 01    05 OY-SINAL16                     PIC  X(001).
417 17    05 OY-VTOT-JURO-DIFCA-CRATS-INT   PIC  9(017).
434 01    05 OY-FILLER43                    PIC  X(001)  VALUE ','.
435 02    05 OY-VTOT-JURO-DIFCA-CRATS-DEC   PIC  9(002).
437 01    05 OY-FILLER44                    PIC  X(001)  VALUE ';'.
438 01    05 OY-SINAL17                     PIC  X(001).
439 17    05 OY-VTOT-JURO-DIFCA-MORA-INT    PIC  9(017).
456 01    05 OY-FILLER45                    PIC  X(001)  VALUE ','.
457 02    05 OY-VTOT-JURO-DIFCA-MORA-DEC    PIC  9(002).
459 01    05 OY-FILLER46                    PIC  X(001)  VALUE ';'.
460 01    05 OY-SINAL18                     PIC  X(001).
461 17    05 OY-VTOT-JURO-VNCER-ACUM-INT    PIC  9(017).
478 01    05 OY-FILLER47                    PIC  X(001)  VALUE ','.
479 02    05 OY-VTOT-JURO-VNCER-ACUM-DEC    PIC  9(002).
481 01    05 OY-FILLER48                    PIC  X(001)  VALUE ';'.
482 01    05 OY-SINAL19                     PIC  X(001).
483 17    05 OY-VTOT-JURO-MORA-ACUM-INT     PIC  9(017).
500 01    05 OY-FILLER49                    PIC  X(001)  VALUE ','.
501 02    05 OY-VTOT-JURO-MORA-ACUM-DEC     PIC  9(002).
503 01    05 OY-FILLER50                    PIC  X(001)  VALUE ';'.
504 01    05 OY-SINAL20                     PIC  X(001).
505 17    05 OY-VTOT-JURO-CRATS-ACUM-INT    PIC  9(017).
522 01    05 OY-FILLER51                    PIC  X(001)  VALUE ','.
523 02    05 OY-VTOT-JURO-CRATS-ACUM-DEC    PIC  9(002).
525 01    05 OY-FILLER52                    PIC  X(001)  VALUE ';'.
526 01    05 OY-SINAL21                     PIC  X(001).
527 17    05 OY-VTOT-CUSTO-DIA-PCELA-INT    PIC  9(017).
544 01    05 OY-FILLER53                    PIC  X(001)  VALUE ','.
545 02    05 OY-VTOT-CUSTO-DIA-PCELA-DEC    PIC  9(002).
547 01    05 OY-FILLER54                    PIC  X(001)  VALUE ';'.
548 01    05 OY-SINAL22                     PIC  X(001).
549 17    05 OY-VTOT-CUSTO-PCELA-DESC-INT   PIC  9(017).
566 01    05 OY-FILLER55                    PIC  X(001)  VALUE ','.
567 02    05 OY-VTOT-CUSTO-PCELA-DESC-DEC   PIC  9(002).
569 01    05 OY-FILLER56                    PIC  X(001)  VALUE ';'.
570 01    05 OY-SINAL23                     PIC  X(001).
571 17    05 OY-VTOT-CUSTO-PCELA-VENCD-INT  PIC  9(017).
588 01    05 OY-FILLER57                    PIC  X(001)  VALUE ','.
589 02    05 OY-VTOT-CUSTO-PCELA-VENCD-DEC  PIC  9(002).
591 01    05 OY-FILLER58                    PIC  X(001)  VALUE ';'.
592 01    05 OY-SINAL24                     PIC  X(001).
593 17    05 OY-VTOT-CUSTO-TRNSF-CRATS-INT  PIC  9(017).
610 01    05 OY-FILLER59                    PIC  X(001)  VALUE ','.
611 02    05 OY-VTOT-CUSTO-TRNSF-CRATS-DEC  PIC  9(002).
613 01    05 OY-FILLER60                    PIC  X(001)  VALUE ';'.
614 01    05 OY-SINAL25                     PIC  X(001).
615 17    05 OY-VTOT-CUSTO-PROVS-CRATS-INT  PIC  9(017).
632 01    05 OY-FILLER61                    PIC  X(001)  VALUE ','.
633 02    05 OY-VTOT-CUSTO-PROVS-CRATS-DEC  PIC  9(002).
635 01    05 OY-FILLER62                    PIC  X(001)  VALUE ';'.
663 01    05 OY-SINAL26                     PIC  X(001).
637 17    05 OY-VTOT-CUSTO-VNCER-ACUM-INT   PIC  9(017).
654 01    05 OY-FILLER63                    PIC  X(001)  VALUE ','.
655 02    05 OY-VTOT-CUSTO-VNCER-ACUM-DEC   PIC  9(002).
657 01    05 OY-FILLER64                    PIC  X(001)  VALUE ';'.
658 01    05 OY-SINAL27                     PIC  X(001).
659 17    05 OY-VTOT-CUSTO-MORA-ACUM-INT    PIC  9(017).
676 01    05 OY-FILLER65                    PIC  X(001)  VALUE ','.
677 02    05 OY-VTOT-CUSTO-MORA-ACUM-DEC    PIC  9(002).
679 01    05 OY-FILLER66                    PIC  X(001)  VALUE ';'.
680 01    05 OY-SINAL28                     PIC  X(001).
681 17    05 OY-VTOT-CUSTO-CRATS-ACUM-INT   PIC  9(017).
698 01    05 OY-FILLER67                    PIC  X(001)  VALUE ','.
699 02    05 OY-VTOT-CUSTO-CRATS-ACUM-DEC   PIC  9(002).
701 01    05 OY-FILLER68                    PIC  X(001)  VALUE ';'.
702 18    05 OY-QTOT-PCELA                  PIC  9(018).
720 11    05 FILLER                         PIC  X(011).
