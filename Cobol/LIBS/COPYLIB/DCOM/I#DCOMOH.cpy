      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOH       *
      * ARQUIVO = DETALHE_LIBERACAO         SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OH-REGISTRO.
 01 24    05 OH-CCHAVE-REG.
 01 04       10 OH-CCTRO-CUSTO          PIC  X(004).
 05 04       10 OH-CEVNTO-ORIGE         PIC  9(004).
 09 08       10 OH-DMOVTO               PIC  9(008).
 17 01       10 OH-CATULZ               PIC  X(001).
 18 01       10 OH-CINDCD-RPROC         PIC  X(001).
 19 06       10 OH-DCOMPT               PIC  9(006).
 25 13    05 OH-CHV-ENTID-DET-LIBRC.
 25 08       10 OH-CCHAVE-OPER-DESC.
 25 03          15 OH-DANO-OPER-DESC   PIC  S9(004)       USAGE COMP-3.
 28 05          15 OH-NSEQ-OPER-DESC   PIC  S9(009)       USAGE COMP-3.
 33 05       10 OH-DEVNTO               PIC  9(008)       USAGE COMP-3.
 38 02    05 OH-NSEQ-LIBRC-DESC         PIC S9(003)       USAGE COMP-3.
 40 02    05 OH-CMEIO-LIBRC-DESC        PIC S9(003)       USAGE COMP-3.
 42 09    05 OH-VLIQ-LIBRC-CREDT        PIC S9(015)V9(02) USAGE COMP-3.
 51 12    05 OH-ID-CCORR-LIBRC-CREDT.
 51 02       10 OH-CBCO-CREDT           PIC S9(003)       USAGE COMP-3.
 53 03       10 OH-CAG-BCRIA-CREDT      PIC S9(005)       USAGE COMP-3.
 56 07       10 OH-CCTA-CREDT           PIC S9(013)       USAGE COMP-3.
 63 38    05 FILLER                     PIC  X(038).
