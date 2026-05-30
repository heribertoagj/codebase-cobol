      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOE       *
      * ARQUIVO = TARIFA_OPERACAO_DESCONTO  SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OE-REGISTRO.
 01 24    05 OE-CCHAVE-REG.
 01 04       10 OE-CCTRO-CUSTO           PIC  X(004).
 05 04       10 OE-CEVNTO-ORIGE          PIC  9(004).
 09 08       10 OE-DMOVTO                PIC  9(008).
 17 01       10 OE-CATULZ                PIC  X(001).
 18 01       10 OE-CINDCD-RPROC          PIC  X(001).
 19 06       10 OE-DCOMPT                PIC  9(006).
 25 13    05 OE-CHV-ENTID-TARIF-OPER-DESC.
 25 08       10 OE-CCHAVE-OPER-DESC.
 25 03          15 OE-DANO-OPER-DESC     PIC S9(004)       USAGE COMP-3.
 28 05          15 OE-NSEQ-OPER-DESC     PIC S9(009)       USAGE COMP-3.
 33 05       10 OE-DEVNTO                PIC  9(008)       USAGE COMP-3.
 38 01    05 OE-CINDCD-PGDOR-ENCAR       PIC  X(001).
 39 12    05 OE-ID-CCORR-DEB-TARIF.
 39 02       10 OE-CBCO-DEB-TARIF        PIC S9(003)       USAGE COMP-3.
 41 03       10 OE-CJUNC-DEPDC-DEB       PIC S9(005)       USAGE COMP-3.
 44 07       10 OE-CCCORR-DEB-TARIF      PIC S9(013)       USAGE COMP-3.
 51 09    05 OE-VTAC-OPER-DESC           PIC S9(015)V9(02) USAGE COMP-3.
 60 09    05 OE-VTARIF-TITLO-OPER-DESC   PIC S9(015)V9(02) USAGE COMP-3.
 69 04    05 OE-CINDCD-MOTVO-EVNTO-ORIGE PIC  X(004).
 73 28    05 FILLER                      PIC  X(028).
