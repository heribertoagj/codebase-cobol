      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTR       *
      * ARQUIVO = CHAVE BAIXA PARCELA       SEQUENCIAL                 *
      * LRECL   = 300 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TR-REGISTRO.
 01 04    05 TR-CCTRO-CUSTO                 PIC  X(004).
 05 04    05 TR-CEVNTO-ORIGE                PIC  9(004).
 09 08    05 TR-DMOVTO                      PIC  9(008).
 17 08    05 TR-DEVNTO                      PIC  9(008).
 25 03    05 TR-CPRODT                      PIC  9(003).
 28 03    05 TR-CSPROD-DESC                 PIC  9(003).
 31 03    05 TR-CTPO-DESC                   PIC  9(003).
 34 05    05 TR-CCART-NORML                 PIC  X(005).
 39 05    05 TR-CCART-VENCD                 PIC  X(005).
 44 03    05 TR-CSIT-PCELA-DESC             PIC  9(003).
 47 05    05 TR-CAG-OPER-DESC               PIC  9(005).
 52 17    05 TR-VSDO-DVDOR-PCELA            PIC S9(015)V99.
 69 17    05 TR-VPRINC-BAIXA-PCELA          PIC S9(015)V99.
 86 17    05 TR-VJURO-BAIXA-PCELA           PIC S9(015)V99.
103 17    05 TR-VIOF-BAIXA-PCELA            PIC S9(015)V99.
120 17    05 TR-VMULTA-BAIXA-PCELA          PIC S9(015)V99.
137 17    05 TR-VABTMT-BAIXA-PCELA          PIC S9(015)V99.
154 17    05 TR-VDESC-BAIXA-PCELA           PIC S9(015)V99.
171 17    05 TR-VEFETV-BAIXA-PCELA          PIC S9(015)V99.
188 17    05 TR-VJURO-MORA-LEGAL            PIC S9(015)V99.
205 17    05 TR-VCUSTO-BAIXA-PCELA          PIC S9(015)V99.
222 17    05 TR-VJURO-BAIXA-PROVS           PIC S9(015)V99.
239 17    05 TR-VCUSTO-BAIXA-PROVS          PIC S9(015)V99.
256 17    05 TR-VDEVLC-BAIXA-EXCED          PIC S9(015)V99.
273 17    05 TR-VRECTA-BAIXA-EXCED          PIC S9(015)V99.
290 11    05 FILLER                         PIC  X(011).
