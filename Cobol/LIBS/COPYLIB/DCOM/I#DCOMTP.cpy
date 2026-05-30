      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTP       *
      * ARQUIVO = CHAVE DETALHE LIBERACAO   SEQUENCIAL                 *
      * LRECL   = 070 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TP-REGISTRO.
 01 04    05 TP-CCTRO-CUSTO                 PIC  X(004).
 05 04    05 TP-CEVNTO-ORIGE                PIC  9(004).
 09 08    05 TP-DMOVTO                      PIC  9(008).
 17 08    05 TP-DEVNTO                      PIC  9(008).
 25 03    05 TP-CPRODT                      PIC  9(003).
 28 03    05 TP-CSPROD-DESC                 PIC  9(003).
 31 03    05 TP-CTPO-DESC                   PIC  9(003).
 34 05    05 TP-CCART-NORML                 PIC  X(005).
 39 05    05 TP-CAG-OPER-DESC               PIC  9(005).
 44 03    05 TP-CMEIO-LIBRC-DESC            PIC  9(003).
 47 17    05 TP-VLIQ-LIBRC-CREDT            PIC S9(015)V99.
 64 07    05 FILLER                         PIC  X(007).
