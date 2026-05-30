      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRX                                      *
      * TAMANHO : 0372 BYTES                                           *
      * ARQUIVO : WORKFLOW CONTRATO LIMITE                             *
      * INCLUDE DA TABELA DCOMB087 - TWORKF_CONTR_LIM                  *
      ******************************************************************
       01  RX-87-REGISTRO.
  1  5     10 RX-87-CCONTR-LIM-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     10 RX-87-CVRSAO-CONTR-LIM     PIC S9(3)V USAGE COMP-3.
  8  2     10 RX-87-CEVNTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
 10 26     10 RX-87-HWORKF-CONTR-LIM     PIC X(26).
 36  2     10 RX-87-CSIT-INIC-DESC       PIC S9(3)V USAGE COMP-3.
 38 26     10 RX-87-HSIT-INIC-DESC       PIC X(26).
 64  2     10 RX-87-CSIT-FNAL-DESC       PIC S9(3)V USAGE COMP-3.
 66 26     10 RX-87-HSIT-FNAL-DESC       PIC X(26).
 92242     10 RX-87-RWORKF-CONTR-LIM.
 92  2        49 RX-87-RWORKF-CONTR-LIM-LEN  PIC S9(4) USAGE COMP.
 94240        49 RX-87-RWORKF-CONTR-LIM-TEXT PIC X(240).
334 26     10 RX-87-HULT-ATULZ           PIC X(26).
360  5     10 RX-87-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
365  8     10 RX-87-CTERM                PIC X(8).
