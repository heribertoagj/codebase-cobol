      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOB       *
      * ARQUIVO = GARANTIA_DESCONTO         SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OB-REGISTRO.
 01 24    05 OB-CCHAVE-REG.
 01 04       10 OB-CCTRO-CUSTO            PIC  X(004).
 05 04       10 OB-CEVNTO-ORIGE           PIC  9(004).
 09 08       10 OB-DMOVTO                 PIC  9(008).
 17 01       10 OB-CATULZ                 PIC  X(001).
 18 01       10 OB-CINDCD-RPROC           PIC  X(001).
 19 06       10 OB-DCOMPT                 PIC  9(006).
 25 10    05 OB-CHV-ENTID-GARNT-OPER-DESC.
 25 08       10 OB-CCHAVE-OPER-DESC.
 25 03          15 OB-DANO-OPER-DESC      PIC S9(004)    COMP-3.
 28 05          15 OB-NSEQ-OPER-DESC      PIC S9(009)    COMP-3.
 33 02       10 OB-CTPO-GARNT-OPER        PIC S9(003)    COMP-3.
 35 02    05 OB-CINDCD-ORIGE-GARNT        PIC  X(002).
 37 04    05 OB-CINDCD-MOTVO-EVNTO-ORIGE  PIC  X(004).
 41 60    05 FILLER                       PIC  X(060).
