      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQH                                      *
      * TAMANHO : 0048 BYTES                                           *
      * ARQUIVO : GARANTIA CONTRATO LIMITE                             *
      * INCLUDE DA TABELA DCOMB0A4- - DB2PRD.TGARNT_CONTR_LIM          *
      ******************************************************************
       01  QH-A4-REGISTRO.
  1  5     05 QH-A4-CCONTR-LIM-DESC    PIC S9(9)V USAGE COMP-3.
  6  2     05 QH-A4-CVRSAO-CONTR-LIM   PIC S9(3)V USAGE COMP-3.
  8  2     05 QH-A4-CTPO-GARNT         PIC S9(3)V USAGE COMP-3.
 10  8     05 QH-A4-CTERM              PIC  X(8).
 18  5     05 QH-A4-CFUNC-BDSCO        PIC S9(9)V USAGE COMP-3.
 23 26     05 QH-A4-HULT-ATULZ         PIC X(26).
