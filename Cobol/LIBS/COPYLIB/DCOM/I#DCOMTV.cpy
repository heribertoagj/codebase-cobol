      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTV       *
      * ARQUIVO = BASE AMOSTRAGENS ISD      SEQUENCIAL                 *
      * LRECL   = 035 BYTES                 12-06-2007                 *
      *----------------------------------------------------------------*
       01 TV-REGISTRO.
 01 03    05 TV-CPRODT                      PIC  9(003).
 04 03    05 TV-CSPROD-DESC                 PIC  9(003).
 07 03    05 TV-CTPO-DESC                   PIC  9(003).
 10 04    05 TV-DANO-OPER-DESC-INI          PIC  9(004).
 14 09    05 TV-NSEQ-OPER-DESC-INI          PIC  9(009).
 23 04    05 TV-DANO-OPER-DESC-FIM          PIC  9(004).
 27 09    05 TV-NSEQ-OPER-DESC-FIM          PIC  9(009).
