      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTU       *
      * ARQUIVO = CHAVE TARIFA CONTRATO     SEQUENCIAL                 *
      * LRECL   = 075 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TU-REGISTRO.
 01 04    05 TU-CCTRO-CUSTO                 PIC  X(004).
 05 04    05 TU-CEVNTO-ORIGE                PIC  9(004).
 09 08    05 TU-DMOVTO                      PIC  9(008).
 17 08    05 TU-DEVNTO                      PIC  9(008).
 25 03    05 TU-CPRODT                      PIC  9(003).
 28 03    05 TU-CSPROD-DESC                 PIC  9(003).
 31 02    05 TU-CINDCD-CONTR-CONVE          PIC  X(002).
 33 05    05 TU-CAG-BCRIA-CONTR-CONVE       PIC  9(005).
 38 17    05 TU-VTAC-CONTR-LIM              PIC S9(015)V99.
 55 17    05 TU-VTARIF-ADTTO-CONTR-LIM      PIC S9(015)V99.
 72 04    05 FILLER                         PIC  X(004).
