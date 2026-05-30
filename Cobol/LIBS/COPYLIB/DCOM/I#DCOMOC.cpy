      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOC       *
      * ARQUIVO = PESSOA DESCONTO           SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OC-REGISTRO.
 01 24    05 OC-CCHAVE-REG.
 01 04       10 OC-CCTRO-CUSTO            PIC  X(004).
 05 04       10 OC-CEVNTO-ORIGE           PIC  9(004).
 09 08       10 OC-DMOVTO                 PIC  9(008).
 17 01       10 OC-CATULZ                 PIC  X(001).
 18 01       10 OC-CINDCD-RPROC           PIC  X(001).
 19 06       10 OC-DCOMPT                 PIC  9(006).
 25 15    05 OC-CHV-ENTID-PSSOA-DESC.
 25 15       10 OC-CCHAVE-OPER-DESC.
 25 03          15 OC-DANO-OPER-DESC      PIC S9(004)      USAGE COMP-3.
 28 05          15 OC-NSEQ-OPER-DESC      PIC S9(009)      USAGE COMP-3.
 33 02          15 OC-CINDCD-PSSOA-DESC   PIC  X(002).
 35 05          15 OC-NRELAC-PSSOA-DESC   PIC S9(009)      USAGE COMP-3.
 40 12    05 OC-ID-CCORR-PSSOA-DESC.
 40 02       10 OC-CBCO-PSSOA             PIC S9(003)      USAGE COMP-3.
 42 03       10 OC-CAG-BCRIA-PSSOA        PIC S9(005)      USAGE COMP-3.
 45 07       10 OC-CCCORR-PSSOA           PIC S9(013)      USAGE COMP-3.
 52 06    05 OC-CCLUB-PSSOA               PIC S9(010)      USAGE COMP-3.
 58 10    05 OC-CNPJ-CPF-PSSOA-DESC.
 58 05       10 OC-CCNPJ-CPF-PSSOA        PIC S9(009)      USAGE COMP-3.
 63 03       10 OC-CFLIAL-CNPJ-PSSOA      PIC S9(005)      USAGE COMP-3.
 66 02       10 OC-CCTRL-CNPJ-PSSOA       PIC S9(002)      USAGE COMP-3.
 68 04    05 OC-CINDCD-MOTVO-EVNTO-ORIGE  PIC  X(004).
 72 29    05 FILLER                       PIC  X(029).
