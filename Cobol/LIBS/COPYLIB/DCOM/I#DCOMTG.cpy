      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTG       *
      * ARQUIVO = CONTROLE DE DATAS         SEQUENCIAL                 *
      * LRECL   = 070 BYTES                 19-04-2007                 *
      *----------------------------------------------------------------*
       01 TG-REGISTRO.
 01 02    10 TG-CPRODT                   PIC S9(003)   USAGE COMP-3.
 03 01    10 TG-CINDCD-PARM-CTRL         PIC S9(001)   USAGE COMP-3.
 04 08    10 TG-DREFT-MOVTO-DIA          PIC  9(008).
 12 10    10 TG-DPROCM-DCOM              PIC  X(010).
 22 10    10 TG-DPROCM-INTER             PIC  X(010).
 32 01    10 TG-CINDCD-RPROC             PIC  X(001).
 33 06    10 TG-DCOMPT                   PIC  9(006).
 39 01    10 TG-CINDCD-FCHTO-COMPT       PIC  X(001).
 40 31    05 FILLER                      PIC  X(031).
