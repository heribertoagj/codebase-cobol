      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSQ                                      *
      * TAMANHO : 0054 BYTES                                           *
      * ARQUIVO : CONVENIO HORA DE REMESSA                             *
      * INCLUDE DA TABELA DCOMB0H2 - TCONVE_HORA_REMSS                 *
      ******************************************************************
       01  SQ-H2-REGISTRO.
  1  5     05 SQ-H2-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SQ-H2-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  8     05 SQ-H2-HARQ-REMSS-CONVN     PIC X(8).
 16 26     05 SQ-H2-HULT-ATULZ           PIC X(26).
 42  5     05 SQ-H2-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 47  8     05 SQ-H2-CTERM                PIC X(8).
