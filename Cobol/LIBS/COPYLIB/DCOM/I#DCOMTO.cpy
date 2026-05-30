      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTO       *
      * ARQUIVO = CHAVE LIBERACAO CREDITO   SEQUENCIAL                 *
      * LRECL   = 120 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TO-REGISTRO.
 01 04    05 TO-CCTRO-CUSTO                      PIC  X(004).
 05 04    05 TO-CEVNTO-ORIGE                     PIC  9(004).
 09 08    05 TO-DMOVTO                           PIC  9(008).
 17 08    05 TO-DEVNTO                           PIC  9(008).
 25 03    05 TO-CPRODT                           PIC  9(003).
 28 03    05 TO-CSPROD-DESC                      PIC  9(003).
 31 03    05 TO-CTPO-DESC                        PIC  9(003).
 34 05    05 TO-CCART-NORML                      PIC  X(005).
 39 03    05 TO-CSIT-OPER-DESC                   PIC  9(003).
 42 05    05 TO-CAG-OPER-DESC                    PIC  9(005).
 47 01    05 TO-CINDCD-TPO-LIBRC-OPER            PIC  X(001).
 48 17    05 TO-VLIBRC-FLUXO-OPER                PIC S9(015)V99.
 65 17    05 TO-VLIBRC-AJUST-OPER                PIC S9(015)V99.
 82 17    05 TO-VLIQ-LIBRC-DESC                  PIC S9(015)V99.
 99 17    05 TO-VRENDA-APROPRI-LIBRC             PIC S9(015)V99.
116 05    05 FILLER                              PIC  X(005).
