      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOF       *
      * ARQUIVO = TRIBUTO_DESCONTO          SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OF-REGISTRO.
 01 24    05 OF-CCHAVE-REG.
 01 04       10 OF-CCTRO-CUSTO           PIC  X(004).
 05 04       10 OF-CEVNTO-ORIGE          PIC  9(004).
 09 08       10 OF-DMOVTO                PIC  9(008).
 17 01       10 OF-CATULZ                PIC  X(001).
 18 01       10 OF-CINDCD-RPROC          PIC  X(001).
 19 06       10 OF-DCOMPT                PIC  9(006).
 25 13    05 OF-CHV-ENTID-TRIBT-OPER-DESC.
 25 08       10 OF-CCHAVE-OPER-DESC.
 25 03          15 OF-DANO-OPER-DESC     PIC S9(004)      USAGE COMP-3.
 28 05          15 OF-NSEQ-OPER-DESC     PIC S9(009)      USAGE COMP-3.
 33 05       10 OF-DEVNTO                PIC  9(008)      USAGE COMP-3.
 38 05    05 OF-PALIQT-IOF-DESC         PIC S9(003)V9(06) USAGE COMP-3.
 43 09    05 OF-VIOF-OPER-DESC          PIC S9(015)V9(02) USAGE COMP-3.
 52 04    05 OF-CINDCD-MOTVO-EVNTO-ORIGE PIC  X(004).
 56 45    05 FILLER                      PIC  X(045).
