      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSL                                      *
      * TAMANHO : 0356 BYTES                                           *
      * ARQUIVO : WORKFLOW CONVENIO CLIENTE                            *
      * INCLUDE DA TABELA DCOMB088 - TWORKF_CONVE_CLI                  *
      ******************************************************************
       01  SL-88-REGISTRO.
  1  5     05 SL-88-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SL-88-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  2     05 SL-88-CEVNTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
 10 10     05 SL-88-HWORKF-CONVE-CLI     PIC X(10).
 20  2     05 SL-88-CSIT-INIC-DESC       PIC S9(3)V USAGE COMP-3.
 22 26     05 SL-88-HSIT-INIC-DESC       PIC X(26).
 48  2     05 SL-88-CSIT-FNAL-DESC       PIC S9(3)V USAGE COMP-3.
 50 26     05 SL-88-HSIT-FNAL-DESC       PIC X(26).
 76242     05 SL-88-RWORKF-CONVE-CLI.
 76  2        49 SL-88-RWORKF-CONVE-CLI-LEN  PIC S9(4) USAGE COMP.
 78240        49 SL-88-RWORKF-CONVE-CLI-TEXT  PIC X(240).
318 26     05 SL-88-HULT-ATULZ           PIC X(26).
344  5     05 SL-88-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
349  8     05 SL-88-CTERM                PIC X(8).
