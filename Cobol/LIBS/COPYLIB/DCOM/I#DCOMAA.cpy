      ******************************************************************
      * SISTEMA : DCOM - I#DCOMAA                                      *
      * TAMANHO : 0100 BYTES                                           *
      * ARQUIVO : COMISSAO CONVENIO CLIENTE                            *
      * INCLUDE DA TABELA DCOMB010 - DB2PRD.TCOMIS_CONVE_CLI           *
      ******************************************************************
       01  AA-10-REGISTRO.
           05 AA-10-CCONVE-CLI-DESC      PIC S9(9)        COMP-3.
           05 AA-10-CVRSAO-CONVE-CLI     PIC S9(3)        COMP-3.
           05 AA-10-DPGTO-COMIS-CONVE    PIC X(10).
           05 AA-10-VPGTO-COMIS-CONVE    PIC S9(15)V9(2)  COMP-3.
           05 AA-10-CINDCD-RELAT-CONVN   PIC X(1).
           05 AA-10-HULT-ATULZ           PIC X(26).
           05 AA-10-CTRANS-PROG-ATULZ    PIC X(8).
           05 FILLER                     PIC X(39).
