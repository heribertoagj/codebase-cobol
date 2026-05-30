      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTI       *
      * ARQUIVO = OPERACOES DESCONTO ENTRADA SEQUENCIAL                *
      * LRECL   = 300 BYTES                 08-05-2007                 *
      *----------------------------------------------------------------*
       01 TI-REGISTRO.
 01 03    05 TI-DANO-OPER-DESC           PIC S9(004)             COMP-3.
 04 05    05 TI-NSEQ-OPER-DESC           PIC S9(009)             COMP-3.
 09 02    05 TI-CSIT-OPER-DESC           PIC S9(003)             COMP-3.
 11 05    05 TI-CCART-VENCD              PIC  X(005).
 16 05    05 TI-CCART-NORML              PIC  X(005).
 21 02    05 TI-CPRODT                   PIC S9(003)             COMP-3.
 23 02    05 TI-CSPROD-DESC              PIC S9(003)             COMP-3.
 25 02    05 TI-CTPO-DESC                PIC S9(003)             COMP-3.
 27 02    05 TI-CELMTO-DESC-COML         PIC S9(003)             COMP-3.
 29 05    05 TI-CCONTR-CONVE-DESC        PIC S9(009)             COMP-3.
 34 02    05 TI-CVRSAO-CONTR-CONVE       PIC S9(003)             COMP-3.
 36 02    05 TI-CBCO-OPER-DESC           PIC S9(003)             COMP-3.
 38 03    05 TI-CAG-OPER-DESC            PIC S9(005)             COMP-3.
 41 02    05 TI-CBCO-CDENT               PIC S9(003)             COMP-3.
 43 03    05 TI-CJUNC-DEPDC-CDENT        PIC S9(005)             COMP-3.
 46 07    05 TI-CCCORR-CDENT             PIC S9(013)             COMP-3.
 53 03    05 TI-CPOSTO-SERVC-CDENT       PIC S9(005)             COMP-3.
 56 05    05 TI-NPSSOA-DESC-COML         PIC S9(009)             COMP-3.
 61 06    05 TI-CCLUB-CDENT              PIC S9(010)             COMP-3.
 67 01    05 TI-CTPO-PSSOA-CDENT         PIC  X(001).
 68 05    05 TI-CCNPJ-CPF-CDENT          PIC S9(009)             COMP-3.
 73 03    05 TI-CFLIAL-CNPJ-CDENT        PIC S9(005)             COMP-3.
 76 02    05 TI-CCTRL-CNPJ-CDENT         PIC S9(002)             COMP-3.
 78 60    05 TI-ICDENT                   PIC  X(060).
138 03    05 TI-DANO-BASE-CONS-CREDT     PIC S9(004)             COMP-3.
141 03    05 TI-CDEPDC-CONS-CREDT        PIC S9(005)             COMP-3.
144 05    05 TI-CSEQ-STUDO-CREDT         PIC S9(009)             COMP-3.
149 09    05 TI-VOPER-DESC               PIC S9(015)V9(02)       COMP-3.
158 10    05 TI-DINIC-OPER-DESC          PIC  X(010).
168 10    05 TI-DVCTO-FNAL-OPER          PIC  X(010).
178 03    05 TI-TOPER-DESC               PIC S9(005)             COMP-3.
181 03    05 TI-TMED-PONDE               PIC S9(005)             COMP-3.
184 02    05 TI-CMEIO-ENTRD-DESC         PIC S9(003)             COMP-3.
186 02    05 TI-CMEIO-PGTO-DESC          PIC S9(003)             COMP-3.
188 05    05 TI-QTITLO-CHEQ-OPER         PIC S9(009)             COMP-3.
193 05    05 TI-PALIQT-IOF-DESC          PIC S9(003)V9(06)       COMP-3.
198 02    05 TI-CTPO-TX-JURO-DESC        PIC S9(003)             COMP-3.
200 05    05 TI-PTX-JURO-ANO             PIC S9(003)V9(06)       COMP-3.
205 05    05 TI-PTX-JURO-MES             PIC S9(003)V9(06)       COMP-3.
210 09    05 TI-VJURO-OPER-DESC          PIC S9(015)V9(02)       COMP-3.
219 05    05 TI-PTX-CUSTO-MES            PIC S9(003)V9(06)       COMP-3.
224 01    05 TI-CINDCD-PGDOR-ENCAR       PIC  X(001).
225 01    05 TI-CINDCD-PGDOR-PCELA       PIC  X(001).
226 08    05 TI-CTERM                    PIC  X(008).
234 10    05 TI-DINCL-REG                PIC  X(010).
244 09    05 TI-VTAC-OPER-DESC           PIC S9(015)V9(02)       COMP-3.
253 09    05 TI-VTARIF-TITLO-OPER-DESC   PIC S9(015)V9(02)       COMP-3.
262 09    05 TI-VLIQ-OPER-DESC           PIC S9(015)V9(02)       COMP-3.
271 02    05 TI-CEVNTO-DESC-COML         PIC S9(003)             COMP-3.
273 09    05 TI-VLIBRC-DESC-COML         PIC S9(015)V9(02)       COMP-3.
282 09    05 TI-VLIBRC-ANTCP-OPER        PIC S9(015)V9(02)       COMP-3.
291 05    05 TI-PTX-CUSTO-ANO            PIC S9(003)V9(06)       COMP-3.
296 03    05 TI-QPCELA-OPER              PIC S9(005)             COMP-3.
299 02    05 FILLER                      PIC  X(002).
