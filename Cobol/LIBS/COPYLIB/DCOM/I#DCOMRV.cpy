      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRV                                      *
      * TAMANHO : 0061 BYTES                                           *
      * ARQUIVO : CONTRATO LIMITE ROTEIRO                              *
      * INCLUDE DA TABELA DCOMB0H1 - TCONTR_LIM_ROTRO                  *
      ******************************************************************
       01  RV-H1-REGISTRO.
  1  5     05 RV-H1-CCONTR-LIM-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 RV-H1-CVRSAO-CONTR-LIM     PIC S9(3)V USAGE COMP-3.
  8  2     05 RV-H1-CTPO-AGPTO-ROTRO     PIC S9(3)V USAGE COMP-3.
 10  1     05 RV-H1-CINDCD-OBRIG-AGPTO   PIC X(1).
 11  1     05 RV-H1-CINDCD-SIT-AGPTO     PIC X(1).
 12  1     05 RV-H1-CINDCD-CONCL-ROTRO   PIC X(1).
 13 10     05 RV-H1-DVALDC-ROTRO-ELMTO   PIC X(10).
 23 26     05 RV-H1-HULT-ATULZ           PIC X(26).
 49  5     05 RV-H1-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 54  8     05 RV-H1-CTERM                PIC X(8).
