      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSK                                      *
      * TAMANHO : 0061 BYTES                                           *
      * ARQUIVO : COMISSAO CONVENIO CLIENTE                            *
      * INCLUDE DA TABELA DCOMB010 - TCOMIS_CONVE_CLI                  *
      ******************************************************************
       01  SK-10-REGISTRO.
  1  5     05 SK-10-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SK-10-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8 10     05 SK-10-DPGTO-COMIS-CONVE    PIC X(10).
 18  9     05 SK-10-VPGTO-COMIS-CONVE    PIC S9(15)V9(2) USAGE COMP-3.
 27  1     05 SK-10-CINDCD-RELAT-CONVN   PIC X(1).
 28 26     05 SK-10-HULT-ATULZ           PIC X(26).
 54  8     05 SK-10-CTRANS-PROG-ATULZ    PIC X(8).
