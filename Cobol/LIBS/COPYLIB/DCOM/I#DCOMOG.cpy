      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOG       *
      * ARQUIVO = LIBERACAO_DESCONTO        SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OG-REGISTRO.
 01 24    05 OG-CCHAVE-REG.
 01 04       10 OG-CCTRO-CUSTO           PIC  X(004).
 05 04       10 OG-CEVNTO-ORIGE          PIC  9(004).
 09 08       10 OG-DMOVTO                PIC  9(008).
 17 01       10 OG-CATULZ                PIC  X(001).
 18 01       10 OG-CINDCD-RPROC          PIC  X(001).
 19 06       10 OG-DCOMPT                PIC  9(006).
 25 13    05 OG-CHV-ENTID-LIBRC-CREDT.
 25 08       10 OG-CCHAVE-OPER-DESC.
 25 03          15 OG-DANO-OPER-DESC     PIC S9(004)       USAGE COMP-3.
 28 05          15 OG-NSEQ-OPER-DESC     PIC S9(009)       USAGE COMP-3.
 33 05       10 OG-DEVNTO                PIC  9(008)       USAGE COMP-3.
 38 01    05 OG-CINDCD-TPO-LIBRC-OPER    PIC  X(001).
 39 09    05 OG-VLIBRC-FLUXO-OPER        PIC S9(015)V9(02) USAGE COMP-3.
 48 09    05 OG-VLIBRC-AJUST-OPER        PIC S9(015)V9(02) USAGE COMP-3.
 57 09    05 OG-VLIQ-OPER-DESC           PIC S9(015)V9(02) USAGE COMP-3.
 66 09    05 OG-VRENDA-APROPRI-LIBRC     PIC S9(015)V9(02) USAGE COMP-3.
 75 01    05 OG-CINDIC-PGDOR-ENCAR       PIC  X(001).
 76 12    05 OG-ID-CCORR-DEB-JURO.
 76 02       10 OG-CBCO-DEB-JURO         PIC S9(003)       USAGE COMP-3.
 78 03       10 OG-CJUNC-DEPDC-DEB       PIC S9(005)       USAGE COMP-3.
 81 07       10 OG-CCCORR-DEB-JURO       PIC S9(013)       USAGE COMP-3.
 88 01    05 OG-CTOP-E-EXCH              PIC  X(001).
 89 12    05 FILLER                      PIC  X(012).
