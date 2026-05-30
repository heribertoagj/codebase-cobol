      ******************************************************************
      * SISTEMA : DCOM - I#DCOMST                                      *
      * TAMANHO : 0372 BYTES                                           *
      * ARQUIVO : WORKFLOW CONVENIO GERAL                              *
      * INCLUDE DA TABELA DCOMB089 - TWORKF_CONVE_GRAL                 *
      ******************************************************************
       01  ST-89-REGISTRO.
  1  5     05 ST-89-CCONVE-GRAL-DESC     PIC S9(9)V USAGE COMP-3.
  6  2     05 ST-89-CVRSAO-CONVE-GRAL    PIC S9(3)V USAGE COMP-3.
  8  2     05 ST-89-CEVNTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
 10 26     05 ST-89-HWORKF-CONVE-GRAL    PIC X(26).
 36  2     05 ST-89-CSIT-INIC-DESC       PIC S9(3)V USAGE COMP-3.
 38 26     05 ST-89-HSIT-INIC-DESC       PIC X(26).
 64  2     05 ST-89-CSIT-FNAL-DESC       PIC S9(3)V USAGE COMP-3.
 66 26     05 ST-89-HSIT-FNAL-DESC       PIC X(26).
 92242     05 ST-89-RWORKF-CONVE-GRAL.
 92  2        49 ST-89-RWORKF-CONVE-GRAL-LEN  PIC S9(4) USAGE COMP.
 94240        49 ST-89-RWORKF-CONVE-GRAL-TEXT  PIC X(240).
334 26     05 ST-89-HULT-ATULZ           PIC X(26).
360  5     05 ST-89-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
365  8     05 ST-89-CTERM                PIC X(8).
