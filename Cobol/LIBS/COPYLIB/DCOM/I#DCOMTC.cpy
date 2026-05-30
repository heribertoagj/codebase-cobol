      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTC       *
      * ARQUIVO = REATIVACAO PARCELA ISD    SEQUENCIAL                 *
      * LRECL   = 440 BYTES                 24-04-2007                 *
      *----------------------------------------------------------------*
       01 TC-REGISTRO.
 01 04    05 TC-CCTRO-CUSTO                      PIC  X(004).
 05 01    05 TC-FILLER1                          PIC  X(001)  VALUE ';'.
 06 04    05 TC-CEVNTC-ORIGE                     PIC  9(004).
 10 01    05 TC-FILLER2                          PIC  X(001)  VALUE ';'.
 11 10    05 TC-DMOVTO                           PIC  X(010).
 21 01    05 TC-FILLER3                          PIC  X(001)  VALUE ';'.
 22 10    05 TC-DEVNTO                           PIC  X(010).
 32 01    05 TC-FILLER4                          PIC  X(001)  VALUE ';'.
 33 03    05 TC-CPRODT                           PIC  9(003).
 36 01    05 TC-FILLER5                          PIC  X(001)  VALUE ';'.
 37 03    05 TC-CSPROD-DESC                      PIC  9(003).
 40 01    05 TC-FILLER6                          PIC  X(001)  VALUE ';'.
 41 03    05 TC-CTPO-DESC                        PIC  9(003).
 44 01    05 TC-FILLER7                          PIC  X(001)  VALUE ';'.
 45 05    05 TC-CCART-NORML                      PIC  X(005).
 50 01    05 TC-FILLER8                          PIC  X(001)  VALUE ';'.
 51 05    05 TC-CCART-VENCD                      PIC  X(005).
 56 01    05 TC-FILLER9                          PIC  X(001)  VALUE ';'.
 57 03    05 TC-CSIT-PCELA-DESC                  PIC  9(003).
 60 01    05 TC-FILLER10                         PIC  X(001)  VALUE ';'.
 61 05    05 TC-CAG-OPER-DESC                    PIC  9(005).
 66 01    05 TC-FILLER11                         PIC  X(001)  VALUE ';'.
 67 18    05 TC-QTOT-TITLO-CHEQ-PCELA            PIC  9(018).
 85 01    05 TC-FILLER12                         PIC  X(001)  VALUE ';'.
 86 01    05 TC-SINAL1                           PIC  X(001).
 87 17    05 TC-VTOT-TARIF-PCELA-DESC-INT        PIC  9(017).
104 01    05 TC-FILLER13                         PIC  X(001)  VALUE ','.
105 02    05 TC-VTOT-TARIF-PCELA-DESC-DEC        PIC  9(002).
107 01    05 TC-FILLER14                         PIC  X(001)  VALUE ';'.
108 01    05 TC-SINAL2                           PIC  X(001).
109 17    05 TC-VTOT-PRINC-PCELA-DESC-INT        PIC  9(017).
126 01    05 TC-FILLER15                         PIC  X(001)  VALUE ','.
127 02    05 TC-VTOT-PRINC-PCELA-DESC-DEC        PIC  9(002).
129 01    05 TC-FILLER16                         PIC  X(001)  VALUE ';'.
130 01    05 TC-SINAL3                           PIC  X(001).
131 17    05 TC-VTOT-IOF-PCELA-DESC-INT          PIC  9(017).
148 01    05 TC-FILLER17                         PIC  X(001)  VALUE ','.
149 02    05 TC-VTOT-IOF-PCELA-DESC-DEC          PIC  9(002).
151 01    05 TC-FILLER18                         PIC  X(001)  VALUE ';'.
152 01    05 TC-SINAL4                           PIC  X(001).
153 17    05 TC-VTOT-JURO-PCELA-DESC-INT         PIC  9(017).
170 01    05 TC-FILLER19                         PIC  X(001)  VALUE ','.
171 02    05 TC-VTOT-JURO-PCELA-DESC-DEC         PIC  9(002).
173 01    05 TC-FILLER20                         PIC  X(001)  VALUE ';'.
174 01    05 TC-SINAL5                           PIC  X(001).
175 17    05 TC-VTOT-JURO-DIA-PCELA-INT          PIC  9(017).
192 01    05 TC-FILLER21                         PIC  X(001)  VALUE ','.
193 02    05 TC-VTOT-JURO-DIA-PCELA-DEC          PIC  9(002).
195 01    05 TC-FILLER22                         PIC  X(001)  VALUE ';'.
196 01    05 TC-SINAL6                           PIC  X(001).
197 17    05 TC-VTOT-JURO-DATA-EVNTO-INT         PIC  9(017).
214 01    05 TC-FILLER23                         PIC  X(001)  VALUE ','.
215 02    05 TC-VTOT-JURO-DATA-EVNTO-DEC         PIC  9(002).
217 01    05 TC-FILLER24                         PIC  X(001)  VALUE ';'.
218 01    05 TC-SINAL7                           PIC  X(001).
219 17    05 TC-VTOT-JURO-APROP-ACUM-INT         PIC  9(017).
236 01    05 TC-FILLER25                         PIC  X(001)  VALUE ','.
237 02    05 TC-VTOT-JURO-APROP-ACUM-DEC         PIC  9(002).
239 01    05 TC-FILLER26                         PIC  X(001)  VALUE ';'.
240 01    05 TC-SINAL8                           PIC  X(001).
241 17    05 TC-VTOT-PSENT-PCELA-DESC-INT        PIC  9(017).
258 01    05 TC-FILLER27                         PIC  X(001)  VALUE ','.
259 02    05 TC-VTOT-PSENT-PCELA-DESC-DEC        PIC  9(002).
261 01    05 TC-FILLER28                         PIC  X(001)  VALUE ';'.
262 01    05 TC-SINAL9                           PIC  X(001).
263 17    05 TC-VTOT-RAP-PSENT-PCELA-INT         PIC  9(017).
280 01    05 TC-FILLER29                         PIC  X(001)  VALUE ','.
281 02    05 TC-VTOT-RAP-PSENT-PCELA-DEC         PIC  9(002).
283 01    05 TC-FILLER30                         PIC  X(001)  VALUE ';'.
284 01    05 TC-SINAL10                          PIC  X(001).
285 17    05 TC-VTOT-SDO-DVDOR-PRINC-INT         PIC  9(017).
302 01    05 TC-FILLER31                         PIC  X(001)  VALUE ','.
303 02    05 TC-VTOT-SDO-DVDOR-PRINC-DEC         PIC  9(002).
305 01    05 TC-FILLER32                         PIC  X(001)  VALUE ';'.
306 01    05 TC-SINAL11                          PIC  X(001).
307 17    05 TC-VTOT-IOF-PCELA-VENCD-INT         PIC  9(017).
324 01    05 TC-FILLER33                         PIC  X(001)  VALUE ','.
325 02    05 TC-VTOT-IOF-PCELA-VENCD-DEC         PIC  9(002).
327 01    05 TC-FILLER34                         PIC  X(001)  VALUE ';'.
328 01    05 TC-SINAL12                          PIC  X(001).
329 17    05 TC-VTOT-JURO-MORA-LEGAL-INT         PIC  9(017).
346 01    05 TC-FILLER35                         PIC  X(001)  VALUE ','.
347 02    05 TC-VTOT-JURO-MORA-LEGAL-DEC         PIC  9(002).
349 01    05 TC-FILLER36                         PIC  X(001)  VALUE ';'.
350 01    05 TC-SINAL13                          PIC  X(001).
351 17    05 TC-VTOT-JURO-MORA-GERC-INT          PIC  9(017).
368 01    05 TC-FILLER37                         PIC  X(001)  VALUE ','.
369 02    05 TC-VTOT-JURO-MORA-GERC-DEC          PIC  9(002).
371 01    05 TC-FILLER38                         PIC  X(001)  VALUE ';'.
372 01    05 TC-SINAL14                          PIC  X(001).
373 17    05 TC-VTOT-JURO-TRNSF-CRATS-INT        PIC  9(017).
390 01    05 TC-FILLER39                         PIC  X(001)  VALUE ','.
391 02    05 TC-VTOT-JURO-TRNSF-CRATS-DEC        PIC  9(002).
393 01    05 TC-FILLER40                         PIC  X(001)  VALUE ';'.
394 18    05 TC-QTOT-PCELA                       PIC  9(018).
412 29    05 FILLER                              PIC  X(029).
