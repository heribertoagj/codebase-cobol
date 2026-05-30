      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMTM       *
      * ARQUIVO = CHAVE TARIFA OPERACAO     SEQUENCIAL                 *
      * LRECL   = 100 BYTES                 30-05-2007                 *
      *----------------------------------------------------------------*
       01 TM-REGISTRO.
 01 04    05 TM-CCTRO-CUSTO                      PIC  X(004).
 05 04    05 TM-CEVNTO-ORIGE                     PIC  9(004).
 09 08    05 TM-DMOVTO                           PIC  9(008).
 17 08    05 TM-DEVNTO                           PIC  9(008).
 25 03    05 TM-CPRODT                           PIC  9(003).
 28 03    05 TM-CSPROD-DESC                      PIC  9(003).
 31 03    05 TM-CTPO-DESC                        PIC  9(003).
 34 05    05 TM-CCART-NORML                      PIC  X(005).
 39 05    05 TM-CCART-VENCD                      PIC  X(005).
 44 03    05 TM-CSIT-OPER-DESC                   PIC  9(003).
 47 05    05 TM-CAG-OPER-DESC                    PIC  9(005).
 52 17    05 TM-VTAC-OPER-DESC                   PIC S9(015)V99.
 69 17    05 TM-VTARIF-TITLO-OPER-DESC           PIC S9(015)V99.
 86 15    05 FILLER                              PIC  X(015).
