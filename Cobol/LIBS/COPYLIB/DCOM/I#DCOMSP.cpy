      ******************************************************************
      * SISTEMA : DCOM - I#DCOMSP                                      *
      * TAMANHO : 0054 BYTES                                           *
      * ARQUIVO : MEIO PAGAMENTO CONVENIO                              *
      * INCLUDE DA TABELA DCOMB035 - TMEIO_PGTO_COVE                   *
      ******************************************************************
       01  SP-35-REGISTRO.
  1  5     05 SP-35-CCONVE-CLI-DESC      PIC S9(9)V USAGE COMP-3.
  6  2     05 SP-35-CVRSAO-CONVE-CLI     PIC S9(3)V USAGE COMP-3.
  8  1     05 SP-35-CINDCD-VCTO-PCELA    PIC X(1).
  9  2     05 SP-35-CMEIO-PGTO-DESC      PIC S9(3)V USAGE COMP-3.
 11  1     05 SP-35-CINDCD-RESP-PGTO     PIC X(1).
 12  2     05 SP-35-TDEB-PCELA-VENCD     PIC S9(3)V USAGE COMP-3.
 14  2     05 SP-35-CINDCD-PRZ-PCELA     PIC X(2).
 16 26     05 SP-35-HULT-ATULZ           PIC X(26).
 42  5     05 SP-35-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
 47  8     05 SP-35-CTERM                PIC X(8).
