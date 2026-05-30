      ******************************************************************
      * SISTEMA : DCOM - I#DCOMR3                                      *
      * TAMANHO : 0067 BYTES                                           *
      * ARQUIVO : PARAMETRO DE PROCESSAMENTO DO SISTEMA DESCONTO       *
      * INCLUDE DA TABELA DCOMB038 - DB2PRD.TPARM_PROCM_DESC           *
      ******************************************************************
       01  R3-38-REGISTRO.
01 02       05 R3-38-CPRODT               PIC S9(3)V   USAGE COMP-3.
03 01       05 R3-38-CINDCD-PARM-CTRL     PIC S9(1)V   USAGE COMP-3.
04 10       05 R3-38-DREFT-MOVTO-DIA      PIC  X(10).
14 10       05 R3-38-DPROCM-ANTER         PIC  X(10).
24 10       05 R3-38-DPROCM-ATUAL         PIC  X(10).
34 10       05 R3-38-DPROCM-POSTR         PIC  X(10).
44 08       05 R3-38-HENCRR-PROCM-ANTER   PIC  X(08).
52 08       05 R3-38-HENCRR-PROCM-ATUAL   PIC  X(08).
60 08       05 R3-38-HENCRR-PROCM-POSTR   PIC  X(08).
