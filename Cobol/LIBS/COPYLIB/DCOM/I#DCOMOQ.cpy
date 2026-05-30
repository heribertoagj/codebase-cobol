      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOQ       *
      * ARQUIVO = TARIFAS CONTRATO          SEQUENCIAL                 *
      * LRECL   = 150 BYTES                 18-04-2007                 *
      *----------------------------------------------------------------*
       01 OQ-REGISTRO.
 01 24    05 OQ-CCHAVE-REG.
 01 04       10 OQ-CCTRO-CUSTO            PIC  X(004).
 05 04       10 OQ-CEVNTO-ORIGE           PIC  9(004).
 09 08       10 OQ-DMOVTO                 PIC  9(008).
 17 01       10 OQ-CATULZ                 PIC  X(001).
 18 01       10 OQ-CINDCD-RPROC           PIC  X(001).
 19 06       10 OQ-DCOMPT                 PIC  9(006).
 25 14    05 OQ-CHV-ENTID-TARIF-CONTR.
 25 09       10 OQ-CCHAVE-ID-CONTR.
 25 02          15 OQ-CINDCD-CONTR        PIC  X(002).
 27 05          15 OQ-CCONTR              PIC S9(009)      USAGE COMP-3.
 32 02          15 OQ-CVRSAO-CONTR        PIC S9(003)      USAGE COMP-3.
 34 05       10 OQ-DEVNTO                 PIC  9(008)      USAGE COMP-3.
 39 02    05 OQ-CPRODT                    PIC S9(003)      USAGE COMP-3.
 41 10    05 OQ-DINIC-CONTR               PIC  X(010).
 51 10    05 OQ-DVCTO-CONTR               PIC  X(010).
 61 05    05 OQ-ID-AG-OPER-CONTR.
 61 02       10 OQ-CBCO-OPER-CONTR        PIC S9(003)      USAGE COMP-3.
 63 03       10 OQ-CAG-OPER-CONTR         PIC S9(005)      USAGE COMP-3.
 66 15    05 OQ-ID-AG-NEGOC-CONTR.
 66 02       10 OQ-CBCO-CONTR             PIC S9(003)      USAGE COMP-3.
 68 03       10 OQ-CAG-BCRIA-CONTR        PIC S9(005)      USAGE COMP-3.
 71 03       10 OQ-CPOSTO-SERVC-CONTR     PIC S9(005)      USAGE COMP-3.
 74 07       10 OQ-CCORR-CONTR            PIC S9(013)      USAGE COMP-3.
 81 17    05 OQ-CARAC-CDENT-CONTR.
 81 06       10 OQ-CCLUB-CONTR            PIC S9(010)      USAGE COMP-3.
 87 01       10 OQ-CTPO-PSSOA-CONTR       PIC  X(001).
 88 10       10 OQ-CNPJ-CPF-CDENT-CONTR.
 88 05          15 OQ-CCNPJ-CPF-CONTR     PIC S9(009)  USAGE COMP-3.
 93 03          15 OQ-CFLIAL-CNPJ-CONTR   PIC S9(005)  USAGE COMP-3.
 96 02          15 OQ-CCTRL-CNPJ-CONTR    PIC S9(002)  USAGE COMP-3.
 98 02    05 OQ-CTPO-COBR-TAC             PIC S9(003)  USAGE COMP-3.
100 09    05 OQ-VTAC-CONTR                PIC S9(015)V9(2) USAGE COMP-3.
109 09    05 OQ-VTARIF-ADTTO-CONTR        PIC S9(015)V9(2) USAGE COMP-3.
118 04    05 OQ-CINDCD-MOTVO-EVNTO-ORIGE  PIC  X(004).
122 29    05 FILLER                       PIC  X(029).
