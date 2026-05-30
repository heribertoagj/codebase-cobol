      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTQ       *
      * ARQUIVO = CHAVE PARCELA OPERACAO    SEQUENCIAL                 *
      * LRECL   = 560 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TQ-REGISTRO.
 01 04    05 TQ-CCTRO-CUSTO                 PIC  X(004).
 05 04    05 TQ-CEVNTO-ORIGE                PIC  9(004).
 09 08    05 TQ-DMOVTO                      PIC  9(008).
 17 08    05 TQ-DEVNTO                      PIC  9(008).
 25 03    05 TQ-CPRODT                      PIC  9(003).
 28 03    05 TQ-CSPROD-DESC                 PIC  9(003).
 31 03    05 TQ-CTPO-DESC                   PIC  9(003).
 34 05    05 TQ-CCART-NORML                 PIC  X(005).
 39 05    05 TQ-CCART-VENCD                 PIC  X(005).
 44 03    05 TQ-CSIT-PCELA-DESC             PIC  9(003).
 47 05    05 TQ-CAG-OPER-DESC               PIC  9(005).
 52 18    05 TQ-QTITLO-CHEQ-PCELA           PIC  9(018).
 70 17    05 TQ-VTARIF-PCELA                PIC S9(015)V99.
 87 17    05 TQ-VPRINC-PCELA-DESC           PIC S9(015)V99.
104 17    05 TQ-VIOF-PCELA-DESC             PIC S9(015)V99.
121 17    05 TQ-VJURO-PCELA-DESC            PIC S9(015)V99.
138 17    05 TQ-VJURO-DIA-PCELA             PIC S9(015)V99.
155 17    05 TQ-VJURO-DATA-EVNTO            PIC S9(015)V99.
172 17    05 TQ-VJURO-APROP-ACUM            PIC S9(015)V99.
189 17    05 TQ-VPSENT-PCELA-DESC           PIC S9(015)V99.
206 17    05 TQ-VRAP-PSENT-PCELA            PIC S9(015)V99.
223 17    05 TQ-VSDO-DVDOR-PRINC            PIC S9(015)V99.
240 17    05 TQ-VIOF-PCELA-VENCD            PIC S9(015)V99.
257 17    05 TQ-VJURO-MORA-LEGAL            PIC S9(015)V99.
274 17    05 TQ-VJURO-MORA-GERC             PIC S9(015)V99.
291 17    05 TQ-VJURO-TRNSF-CRATS           PIC S9(015)V99.
308 17    05 TQ-VJURO-PROVS-CRATS           PIC S9(015)V99.
325 17    05 TQ-VJURO-DIFCA-CRATS           PIC S9(015)V99.
342 17    05 TQ-VJURO-DIFCA-MORA            PIC S9(015)V99.
359 17    05 TQ-VJURO-VNCER-ACUM            PIC S9(015)V99.
376 17    05 TQ-VJURO-MORA-ACUM             PIC S9(015)V99.
393 17    05 TQ-VJURO-CRATS-ACUM            PIC S9(015)V99.
410 17    05 TQ-VCUSTO-DIA-PCELA            PIC S9(015)V99.
427 17    05 TQ-VCUSTO-PCELA-DESC           PIC S9(015)V99.
444 17    05 TQ-VCUSTO-PCELA-VENCD          PIC S9(015)V99.
461 17    05 TQ-VCUSTO-TRNSF-CRATS          PIC S9(015)V99.
478 17    05 TQ-VCUSTO-PROVS-CRATS          PIC S9(015)V99.
495 17    05 TQ-VCUSTO-VNCER-ACUM           PIC S9(015)V99.
512 17    05 TQ-VCUSTO-MORA-ACUM            PIC S9(015)V99.
529 17    05 TQ-VCUSTO-CRATS-ACUM           PIC S9(015)V99.
546 15    05 FILLER                         PIC  X(015).
