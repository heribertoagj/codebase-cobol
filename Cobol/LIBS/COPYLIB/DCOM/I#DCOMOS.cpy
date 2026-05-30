      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMOS       *
      * ARQUIVO = TOTAIS_CONTROLE           SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 19-06-2007                 *
      *----------------------------------------------------------------*
       01 OS-REGISTRO.
 01 24    05 OS-CCHAVE-REG.
 01 04       10 OS-CCTRO-CUSTO             PIC  X(004).
 05 04       10 OS-CEVNTO-ORIGE            PIC  9(004).
 09 08       10 OS-DMOVTO                  PIC  9(008).
 17 01       10 OS-CINDCD-RPROC            PIC  X(001).
 18 06       10 OS-DCOMPT                  PIC  9(006).
 24 09    05 OS-QTOT-REG-ATULZ-INCL        PIC S9(017)  USAGE COMP-3.
 33 09    05 OS-QTOT-REG-ATULZ-ALT         PIC S9(017)  USAGE COMP-3.
 42 09    05 OS-QTOT-REG-ATULZ-EXCL        PIC S9(017)  USAGE COMP-3.
 51 09    05 OS-QTOT-REG-GRAV-EVTO-ORIGE   PIC S9(017)  USAGE COMP-3.
 60 41    05 FILLER                        PIC  X(041).
