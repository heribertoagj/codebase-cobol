      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOA       *
      * ARQUIVO = OPERACOES DESCONTO        SEQUENCIAL                 *
      * LRECL   = 500 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OA-REGISTRO.
 01 24    05 OA-CCHAVE-REG.
 01 04       10 OA-CCTRO-CUSTO                PIC  X(004).
 05 04       10 OA-CEVNTO-ORIGE               PIC  9(004).
 09 08       10 OA-DMOVTO                     PIC  9(008).
 17 01       10 OA-CATULZ                     PIC  X(001).
 18 01       10 OA-CINDCD-RPROC               PIC  X(001).
 19 06       10 OA-DCOMPT                     PIC  9(006).
 25 08    05 OA-CHV-ENTID-OPER-DESC.
 25 08       10 OA-CCHAVE-OPER-DESC.
 25 03          15 OA-DANO-OPER-DESC          PIC S9(004)        COMP-3.
 28 05          15 OA-NSEQ-OPER-DESC          PIC S9(009)        COMP-3.
 33 02    05 OA-CSIT-OPER-DESC                PIC S9(003)        COMP-3.
 35 05    05 OA-CCART-VENCD                   PIC  X(005).
 40 05    05 OA-CCART-NORML                   PIC  X(005).
 45 04    05 OA-ID-SPROD-DESC.
 45 02       10 OA-CPRODT                     PIC S9(003)        COMP-3.
 47 02       10 OA-CSPROD-DESC                PIC S9(003)        COMP-3.
 49 02    05 OA-CTPO-DESC                     PIC S9(003)        COMP-3.
 51 03    05 OA-CTX-DESC-VNCER                PIC S9(005)        COMP-3.
 54 03    05 OA-CTX-DESC-MORA                 PIC S9(005)        COMP-3.
 57 03    05 OA-CTX-DESC-CRATS                PIC S9(005)        COMP-3.
 60 09    05 OA-ID-CONTR-CONVE.
 60 02       10 OA-CINDCD-CONTR-CONVE         PIC  X(002).
 62 05       10 OA-CCONTR-CONVE-DESC          PIC S9(009)        COMP-3.
 67 02       10 OA-CVRSAO-CONTR-CONVE         PIC S9(003)        COMP-3.
 69 01    05 OA-CRENTB-CONTR-CONVE            PIC  X(001).
 70 01    05 OA-CMODLD-INCL-CONTR-CONVE       PIC  X(001).
 71 05    05 OA-ID-AG-OPER-CONTR-CONVE.
 71 02       10 OA-CBCO-OPER-CONTR-CONVE      PIC S9(003)        COMP-3.
 73 03       10 OA-CAG-OPER-CONTR-CONVE       PIC S9(005)        COMP-3.
 76 15    05 OA-ID-AG-NEGOC-CONTR-CONVE.
 76 02       10 OA-CBCO-CONTR-CONVE           PIC S9(003)        COMP-3.
 78 03       10 OA-CAG-BCRIA-CONTR-CONVE      PIC S9(005)        COMP-3.
 81 03       10 OA-CPOSTO-SERVC-CONTR-CONVE   PIC S9(005)        COMP-3.
 84 07       10 OA-CCCORR-CONTR-CONVE         PIC S9(013)        COMP-3.
 91 17    05 OA-CARAC-CDENT-CONTR-CONVE.
 91 06       10 OA-CCLUB-CONTR-CONVE          PIC S9(010)        COMP-3.
 97 01       10 OA-CTPO-PSSOA-CONTR-CONVE     PIC  X(001).
 98 10       10 OA-CNPJ-CPF-CDENT-CONTR-CONVE.
 98 05          15 OA-CCNPJ-CPF-CONTR-CONVE   PIC S9(009)        COMP-3.
103 03          15 OA-CFLIAL-CNPJ-CONTR-CONVE PIC S9(005)        COMP-3.
106 02          15 OA-CCTRL-CNPJ-CONTR-CONVE  PIC S9(002)        COMP-3.
108 05    05 OA-ID-AG-OPER-DESC.
108 02       10 OA-CBCO-OPER-DESC             PIC S9(003)        COMP-3.
110 03       10 OA-CAG-OPER-DESC              PIC S9(005)        COMP-3.
113 19    05 OA-CARAC-CCORR-CDENT-OPER-DESC.
113 02       10 OA-CBCO-CDENT                 PIC S9(003)        COMP-3.
115 03       10 OA-CJUNC-DEPDC-CDENT          PIC S9(005)        COMP-3.
118 02       10 OA-CGRP-CTBIL-CDENT           PIC S9(003)        COMP-3.
120 02       10 OA-CSGRP-CTBIL-CDENT          PIC S9(003)        COMP-3.
122 07       10 OA-CCCORR-CDENT               PIC S9(013)        COMP-3.
129 03       10 OA-CPOSTO-SERVC-CDENT         PIC S9(005)        COMP-3.
132 81    05 OA-DADOS-CAD-CDENT.
132 06       10 OA-CCLUB-CDENT                PIC S9(010)        COMP-3.
138 01       10 OA-CTPO-PSSOA-CDENT           PIC  X(001).
139 10       10 OA-CNPJ-CPF-CDENT-OPER-DESC.
139 05          15 OA-CCNPJ-CPF-CDENT         PIC S9(009)        COMP-3.
144 03          15 OA-CFLIAL-CNPJ-CDENT       PIC S9(005)        COMP-3.
147 02          15 OA-CCTRL-CNPJ-CDENT        PIC S9(002)        COMP-3.
149 60       10 OA-ICDENT                     PIC  X(060).
209 02       10 OA-CNATUZ-JURID-CDENT         PIC S9(003)        COMP-3.
211 02       10 OA-CRAMO-ATVDD-CDENT          PIC S9(003)        COMP-3.
213 11    05 OA-ID-CONS-CREDT-OPER-DESC.
213 03       10 OA-DANO-BASE-CONS-CREDT       PIC S9(004)        COMP-3.
216 03       10 OA-CDEPDC-CONS-CREDT          PIC S9(005)        COMP-3.
219 05       10 OA-CSEQ-STUDO-CREDT           PIC S9(009)        COMP-3.
224 09    05 OA-VOPER-DESC               PIC S9(015)V9(02)       COMP-3.
233 10    05 OA-DINIC-OPER-DESC               PIC  X(010).
243 10    05 OA-DVCTO-FNAL-OPER               PIC  X(010).
253 03    05 OA-TOPER-DESC                    PIC S9(005)        COMP-3.
256 03    05 OA-TMED-CONTR                    PIC S9(005)        COMP-3.
259 03    05 OA-TMED-OPER                     PIC S9(005)        COMP-3.
262 02    05 OA-CMEIO-ENTRD-DESC              PIC S9(003)        COMP-3.
264 02    05 OA-CMEIO-PGTO-DESC               PIC S9(003)        COMP-3.
266 05    05 OA-QTITLO-CHEQ-OPER              PIC S9(009)        COMP-3.
271 05    05 OA-PALIQT-IOF-DESC          PIC S9(003)V9(06)       COMP-3.
276 02    05 OA-CTPO-TX-JURO-DESC             PIC S9(003)        COMP-3.
278 01    05 OA-CINDCD-CALC-JURO              PIC  X(001).
279 05    05 OA-PTX-JURO-ANO-CONTR       PIC S9(003)V9(06)       COMP-3.
284 05    05 OA-PTX-JURO-ANO             PIC S9(003)V9(06)       COMP-3.
289 05    05 OA-PTX-JURO-MES-CONTR       PIC S9(003)V9(06)       COMP-3.
294 05    05 OA-PTX-JURO-MES             PIC S9(003)V9(06)       COMP-3.
299 05    05 OA-PTX-JURO-MES-POSTC-CONTR PIC S9(003)V9(06)       COMP-3.
304 05    05 OA-PTX-JURO-MES-POSTC-OPER  PIC S9(003)V9(06)       COMP-3.
309 09    05 OA-VJURO-OPER-DESC          PIC S9(015)V9(02)       COMP-3.
318 05    05 OA-PTX-CUSTO-MES-CONTR      PIC S9(003)V9(06)       COMP-3.
323 05    05 OA-PTX-CUSTO-MES-OPER       PIC S9(003)V9(06)       COMP-3.
328 05    05 OA-PSPREAD-CONTR            PIC S9(003)V9(06)       COMP-3.
333 05    05 OA-PSPREAD-OPER             PIC S9(003)V9(06)       COMP-3.
338 01    05 OA-CINDCD-PGDOR-ENCAR       PIC  X(001).
339 01    05 OA-CINDCD-PGDOR-PCELA       PIC  X(001).
340 08    05 OA-CTERM                    PIC  X(008).
348 04    05 OA-CINDCD-MOTVO-EVNTO-ORIGE PIC  X(004).
352 02    05 OA-CGRP-CTBIL-CONV          PIC  9(003)             COMP-3.
354 02    05 OA-CSGRP-CTBIL-CONV         PIC  9(003)             COMP-3.
356 30    05 OA-IPSSOA-CONV              PIC  X(030).
386 09    05 OA-VALOR-LIBERADO           PIC S9(015)V9(02)       COMP-3.
395 05    05 OA-PTX-CUSTO-ANO-CONTR      PIC S9(003)V9(06)       COMP-3.
400 05    05 OA-PTX-CUSTO-ANO-OPER       PIC S9(003)V9(06)       COMP-3.
405 03    05 OA-QPCELA-OPER              PIC S9(005)             COMP-3.
      *----------------------------------------------------------------*
      *-- CAMPOS UTILIZADO NO PROCESSO DE CARGA DO BDCO              --*
      *-- SOMENTE PARA EFEITO DE PERFORMANCE, NAO SERAO ENVIADOS     --*
      *----------------------------------------------------------------*
408 05    05 OA-NPSSOA-DESC-COML-X.
408 05       10 OA-NPSSOA-DESC-COML      PIC S9(009)             COMP-3.
413 28    05 OA-DADOS-CAD-EMITENTE.
413 01       10 OA-CINDCD-CORRENTISTA    PIC  X(001).
414 06       10 OA-CCLUB-EMITENTE        PIC S9(010)             COMP-3.
420 01       10 OA-CTPO-PSSOA-EMITENTE   PIC  X(001).
421 10       10 OA-CNPJ-CPF-EMIT-OPER-DESC.
421 05          15 OA-CCNPJ-CPF-EMITEN   PIC S9(009)             COMP-3.
426 03          15 OA-CFLIAL-CNPJ-EMITEN PIC S9(005)             COMP-3.
429 02          15 OA-CCTRL-CNPJ-EMITEN  PIC S9(002)             COMP-3.
431 03       10 OA-CAG-EMITEN-OPER       PIC S9(005)             COMP-3.
434 07       10 OA-CTA-EMITEN-OPER       PIC S9(013)             COMP-3.
441 60    05 FILLER                      PIC  X(060).
