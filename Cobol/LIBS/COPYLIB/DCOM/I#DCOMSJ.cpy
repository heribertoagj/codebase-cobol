      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSJ                                      *
      * TAMANHO : 0061 BYTES                                           *
      * ARQUIVO : CONVENIO CLIENTE ROTEIRO                             *
      * INCLUDE DA TABELA DCOMB013 - TCONVE_CLI_ROTRO                  *
      ******************************************************************
       01  SJ-13-REGISTRO.
  1  5     05 SJ-13-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SJ-13-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  2     05 SJ-13-CTPO-AGPTO-ROTRO     PIC S9(3)V USAGE COMP-3.
 10  1     05 SJ-13-CINDCD-OBRIG-AGPTO   PIC X(1).
 11  1     05 SJ-13-CINDCD-SIT-AGPTO     PIC X(1).
 12  1     05 SJ-13-CINDCD-CONCL-ROTRO   PIC X(1).
 13 10     05 SJ-13-DVALDC-ROTRO-ELMTO   PIC X(10).
 23 26     05 SJ-13-HULT-ATULZ           PIC X(26).
 49  5     05 SJ-13-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 54  8     05 SJ-13-CTERM                PIC X(8).
