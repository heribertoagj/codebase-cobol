      **---------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM                BOOK    =  I#DCOMTK       *
      * ARQUIVO = REATIVACAO PARCELA ENTRADA SEQUENCIAL                *
      * LRECL   = 200 BYTES                  08-05-2007                *
      *----------------------------------------------------------------*
       01 TK-REGISTRO.
 01 03    05 TK-DANO-OPER-DESC           PIC S9(004)       USAGE COMP-3.
 04 05    05 TK-NSEQ-OPER-DESC           PIC S9(009)       USAGE COMP-3.
 09 03    05 TK-NPCELA-DESC              PIC S9(005)       USAGE COMP-3.
 12 05    05 TK-DEVNTO                   PIC  9(008)       USAGE COMP-3.
 17 26    05 TK-HREATV-PCELA-DESC        PIC  X(026).
 43 04    05 TK-CCONTR-CORP-CHEQC-PCELA  PIC S9(007)       USAGE COMP-3.
 47 02    05 TK-CBCO-CLI-CHEQC           PIC S9(003)       USAGE COMP-3.
 49 03    05 TK-CDEPC-CLI-CHEQC          PIC S9(005)       USAGE COMP-3.
 52 07    05 TK-CCCORR-CLI-CHEQC         PIC S9(013)       USAGE COMP-3.
 59 03    05 TK-CFLIAL-EMPR-CHEQC        PIC S9(005)       USAGE COMP-3.
 62 02    05 TK-CBCO-CHEQ-ANTCP          PIC S9(003)       USAGE COMP-3.
 64 03    05 TK-CAG-CHEQ-ANTCP           PIC S9(005)       USAGE COMP-3.
 67 07    05 TK-CCCORR-CHEQ-ANTCP        PIC S9(013)       USAGE COMP-3.
 74 04    05 TK-CCHEQ-ANTCP              PIC S9(007)       USAGE COMP-3.
 78 10    05 TK-DEFETV-BAIXA-PCELA       PIC  X(010).
 88 09    05 TK-VEFETV-BAIXA-PCELA       PIC S9(015)V9(2)  USAGE COMP-3.
 97 09    05 TK-VIOF-BAIXA-PCELA         PIC S9(015)V9(2)  USAGE COMP-3.
106 95    05 FILLER                      PIC  X(095).
