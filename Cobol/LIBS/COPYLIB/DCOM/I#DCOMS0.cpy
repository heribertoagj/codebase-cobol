      ******************************************************************
      * SISTEMA : DCOM - I#DCOMS0                                      *
      * TAMANHO : 0052 BYTES                                           *
      * ARQUIVO : CONTROLE NUMERO CORPORATIVO                          *
      * INCLUDE DA TABELA DCOMB0J5 - DB2PRD.TCTRL_NRO_CORP             *
      ******************************************************************
       01  S0-J5-REGISTRO.
 01 02     05 S0-J5-CBCO               PIC S9(003) COMP-3.
 03 03     05 S0-J5-CAG-BCRIA          PIC S9(005) COMP-3.
 06 07     05 S0-J5-CCTA-BCRIA-CLI     PIC S9(013) COMP-3.
 13 10     05 S0-J5-NULT-SEQ-CONTR     PIC S9(018) COMP-3.
 23 01     05 S0-J5-NULT-SEQ-NULL      PIC  X(001).
 24 02     05 S0-J5-QDIG-SEQ-CONTR     PIC S9(003) COMP-3.
 26 01     05 S0-J5-QDIG-SEQ-NULL      PIC  X(001).
 27 26     05 S0-J5-HULT-ATULZ         PIC  X(026).
