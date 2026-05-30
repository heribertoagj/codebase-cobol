      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTT       *
      * ARQUIVO = CHAVE SALDO OPERACAO      SEQUENCIAL                 *
      * LRECL   = 420 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TT-REGISTRO.
 01 04    05 TT-CCTRO-CUSTO                PIC  X(004).
 05 04    05 TT-CEVNTO-ORIGE               PIC  9(004).
 09 08    05 TT-DMOVTO                     PIC  X(008).
 17 08    05 TT-DEVNTO                     PIC  X(008).
 25 03    05 TT-CPRODT                     PIC  9(003).
 28 03    05 TT-CSPROD-DESC                PIC  9(003).
 31 03    05 TT-CTPO-DESC                  PIC  9(003).
 34 05    05 TT-CCART-NORML                PIC  X(005).
 39 05    05 TT-CCART-VENCD                PIC  X(005).
 44 03    05 TT-CSIT-PCELA-DESC            PIC  9(003).
 47 05    05 TT-CAG-OPER-DESC              PIC  9(005).
 52 17    05 TT-VPSENT-OPER-DESC           PIC S9(015)V99.
 69 17    05 TT-VRAP-PSENT-OPER-DESC       PIC S9(015)V99.
 86 17    05 TT-VJURO-APROP-ACUM-OPER      PIC S9(015)V99.
103 17    05 TT-VSDO-DVDOR-OPER            PIC S9(015)V99.
120 17    05 TT-VSDO-DVDOR-CRATS           PIC S9(015)V99.
137 17    05 TT-VJURO-MORA-LEGAL-OPER      PIC S9(015)V99.
154 17    05 TT-VJURO-MORA-GERC-OPER       PIC S9(015)V99.
171 17    05 TT-VJURO-TRNSF-OPER           PIC S9(015)V99.
188 17    05 TT-VJURO-PROVS-OPER           PIC S9(015)V99.
205 17    05 TT-VDIFCA-COBR-MORA           PIC S9(015)V99.
222 17    05 TT-VDIFCA-COBR-CRATS          PIC S9(015)V99.
239 17    05 TT-VCUSTO-OPER-VNCER          PIC S9(015)V99.
256 17    05 TT-VCUSTO-OPER-VENCD          PIC S9(015)V99.
273 17    05 TT-VCUSTO-TRNSF-OPER          PIC S9(015)V99.
290 17    05 TT-VCUSTO-PROVS-OPER          PIC S9(015)V99.
307 17    05 TT-VJURO-VNCER-OPER           PIC S9(015)V99.
324 17    05 TT-VJURO-MORA-OPER            PIC S9(015)V99.
341 17    05 TT-VJURO-CRATS-OPER           PIC S9(015)V99.
358 17    05 TT-VCUSTO-VNCER-OPER          PIC S9(015)V99.
375 17    05 TT-VCUSTO-MORA-OPER           PIC S9(015)V99.
392 17    05 TT-VCUSTO-CRATS-OPER          PIC S9(015)V99.
409 12    05 FILLER                        PIC  X(012).
