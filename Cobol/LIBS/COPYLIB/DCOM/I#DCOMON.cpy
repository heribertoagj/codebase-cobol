      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMON       *
      * ARQUIVO = CONTROLE_CONTAS_CONTABEIS SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 09-04-2007                 *
      *----------------------------------------------------------------*
       01 ON-REGISTRO.
 01 24    05 ON-CCHAVE-REG.
 01 04       10 ON-CCTRO-CUSTO            PIC  X(004).
 05 04       10 ON-CEVNTO-ORIGE           PIC  9(004).
 09 08       10 ON-DMOVTO                 PIC  9(008).
 17 01       10 ON-CATULZ                 PIC  X(001).
 18 01       10 ON-CINDCD-RPROC           PIC  X(001).
 19 06       10 ON-DCOMPT                 PIC  9(006).
 25 16    05 ON-CHV-ENTID-CTRL-CTA.
 25 11       10 ON-CCHAVE-CTA-CTBIL.
 25 03          15 ON-CEMPR               PIC S9(005)    USAGE COMP-3.
 28 02          15 ON-CGRP-CTBIL          PIC S9(003)    USAGE COMP-3.
 30 02          15 ON-CSGRP-CTBIL         PIC S9(003)    USAGE COMP-3.
 32 04          15 ON-CCTA-CTBIL          PIC S9(007)    USAGE COMP-3.
 36 05       10 ON-DEVNTO                 PIC  9(008)    USAGE COMP-3.
 41 60    05 FILLER                       PIC  X(060).
