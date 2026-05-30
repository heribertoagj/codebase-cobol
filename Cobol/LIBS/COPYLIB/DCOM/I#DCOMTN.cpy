      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTN       *
      * ARQUIVO = CHAVE TRIBUTO OPERACAO    SEQUENCIAL                 *
      * LRECL   = 080 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TN-REGISTRO.
 01 04    05 TN-CCTRO-CUSTO                 PIC  X(004).
 05 04    05 TN-CEVNTO-ORIGE                PIC  9(004).
 09 08    05 TN-DMOVTO                      PIC  9(008).
 17 08    05 TN-DEVNTO                      PIC  9(008).
 25 03    05 TN-CPRODT                      PIC  9(003).
 28 03    05 TN-CSPROD-DESC                 PIC  9(003).
 31 03    05 TN-CTPO-DESC                   PIC  9(003).
 34 05    05 TN-CCART-NORML                 PIC  X(005).
 39 05    05 TN-CCART-VENCD                 PIC  X(005).
 44 03    05 TN-CSIT-OPER-DESC              PIC  9(003).
 47 05    05 TN-CAG-OPER-DESC               PIC  9(005).
 52 17    05 TN-VIOF-OPER-DESC              PIC S9(015)V99.
 69 12    05 FILLER                         PIC  X(012).
