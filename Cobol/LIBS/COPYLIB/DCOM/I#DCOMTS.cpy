      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTS       *
      * ARQUIVO = CHAVE REATIVACAO PARCELA  SEQUENCIAL                 *
      * LRECL   = 320 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TS-REGISTRO.
 01 04    05 TS-CCTRO-CUSTO                 PIC  X(004).
 05 04    05 TS-CEVNTO-ORIGE                PIC  9(004).
 09 08    05 TS-DMOVTO                      PIC  9(008).
 17 08    05 TS-DEVNTO                      PIC  9(008).
 25 03    05 TS-CPRODT                      PIC  9(003).
 28 03    05 TS-CSPROD-DESC                 PIC  9(003).
 31 03    05 TS-CTPO-DESC                   PIC  9(003).
 34 05    05 TS-CCART-NORML                 PIC  X(005).
 39 05    05 TS-CCART-VENCD                 PIC  X(005).
 44 03    05 TS-CSIT-PCELA-DESC             PIC  9(003).
 47 05    05 TS-CAG-OPER-DESC               PIC  9(005).
 52 18    05 TS-QTITLO-CHEQ-PCELA           PIC  9(018).
 70 17    05 TS-VTARIF-PCELA-DESC           PIC S9(015)V99.
 87 17    05 TS-VPRINC-PCELA-DESC           PIC S9(015)V99.
104 17    05 TS-VIOF-PCELA-DESC             PIC S9(015)V99.
121 17    05 TS-VJURO-PCELA-DESC            PIC S9(015)V99.
138 17    05 TS-VJURO-DIA-PCELA-DESC        PIC S9(015)V99.
155 17    05 TS-VJURO-DATA-EVNTO            PIC S9(015)V99.
172 17    05 TS-VJURO-APROP-ACUM            PIC S9(015)V99.
189 17    05 TS-VPSENT-PCELA-DESC           PIC S9(015)V99.
206 17    05 TS-VRAP-PSENT-PCELA            PIC S9(015)V99.
223 17    05 TS-VSDO-DVDOR-PRINC            PIC S9(015)V99.
240 17    05 TS-VIOF-PCELA-VENCD            PIC S9(015)V99.
257 17    05 TS-VJURO-MORA-LEGAL            PIC S9(015)V99.
274 17    05 TS-VJURO-MORA-GERC             PIC S9(015)V99.
291 17    05 TS-VJURO-TRNSF-CRATS           PIC S9(015)V99.
308 13    05 FILLER                         PIC  X(013).
