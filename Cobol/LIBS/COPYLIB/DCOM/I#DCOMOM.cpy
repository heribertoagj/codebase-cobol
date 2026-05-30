      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOM       *
      * ARQUIVO = INFORMACAO CONTABIL SALDO SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OM-REGISTRO.
 01 24    05 OM-CCHAVE-REG.
 01 04       10 OM-CCTRO-CUSTO            PIC  X(004).
 05 04       10 OM-CEVNTO-ORIGE           PIC  9(004).
 09 08       10 OM-DMOVTO                 PIC  9(008).
 17 01       10 OM-CATULZ                 PIC  X(001).
 18 01       10 OM-CINDCD-RPROC           PIC  X(001).
 19 06       10 OM-DCOMPT                 PIC  9(006).
 25 13    05 OM-CHV-ENTID-INFO-CTBIL.
 25 08       10 OM-CCHAVE-OPER-DESC.
 25 03          15 OM-DANO-OPER-DESC      PIC S9(004)          COMP-3.
 28 05          15 OM-NSEQ-OPER-DESC      PIC S9(009)          COMP-3.
 33 05       10 OM-DEVNTO                 PIC  9(008)          COMP-3.
 38 10    05 OM-CIDTFD-DADO-ORIGE         PIC  X(010).
 48 14    05 OM-INFO-CTA-CTBIL.
 48 03       10 OM-CEMPR-INC              PIC S9(005)          COMP-3.
 51 03       10 OM-CJUNC-CTBIL            PIC S9(005)          COMP-3.
 54 02       10 OM-CGRP-CTBIL             PIC S9(003)          COMP-3.
 56 02       10 OM-CSGRP-CTBIL            PIC S9(003)          COMP-3.
 58 04       10 OM-CCTA-CTBIL             PIC S9(007)          COMP-3.
 62 02    05 OM-CTPO-CTA-CONC             PIC S9(003)          COMP-3.
 64 05    05 OM-DCTBIL                    PIC  9(008)          COMP-3.
 69 03    05 OM-NPCELA-DESC               PIC S9(005)          COMP-3.
 72 29    05 FILLER                       PIC  X(029).
