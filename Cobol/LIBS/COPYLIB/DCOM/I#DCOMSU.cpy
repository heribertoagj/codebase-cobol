      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSU                                      *
      * TAMANHO : 0061 BYTES                                           *
      * ARQUIVO : CONVENIO GERAL ROTEIRO                               *
      * INCLUDE DA TABELA DCOMB017 - TCONVE_GRAL_ROTRO                 *
      ******************************************************************
       01  SU-17-REGISTRO.
 1  5      05 SU-17-CCONVE-GRAL-DESC     PIC S9(9)V USAGE COMP-3.
 6  2      05 SU-17-CVRSAO-CONVE-GRAL    PIC S9(3)V USAGE COMP-3.
 8  2      05 SU-17-CTPO-AGPTO-ROTRO     PIC S9(3)V USAGE COMP-3.
10  1      05 SU-17-CINDCD-OBRIG-AGPTO   PIC X(1).
11  1      05 SU-17-CINDCD-SIT-AGPTO     PIC X(1).
12  1      05 SU-17-CINDCD-CONCL-ROTRO   PIC X(1).
13 10      05 SU-17-DVALDC-ROTRO-ELMTO   PIC X(10).
23 26      05 SU-17-HULT-ATULZ           PIC X(26).
49  5      05 SU-17-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
54  8      05 SU-17-CTERM                PIC X(8).
