      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPQ                                      *
      * TAMANHO : 0038 BYTES                                           *
      * ARQUIVO : TRANSITORIA OPERACAO INSENCAO                        *
      * INCLUDE DA TABELA DCOMB0I0 - TTRASI_OPER_ISENC                 *
      ******************************************************************
       01  PQ-I0-REGISTRO.
01  03     05 PQ-I0-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04  05     05 PQ-I0-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09  02     05 PQ-I0-CTPO-ISENC-DESC      PIC S9(3)V USAGE COMP-3.
11  26     05 PQ-I0-HULT-ATULZ           PIC X(26).
37  02     05 PQ-I0-CABRGC-ISENC-DESC    PIC S9(3)V USAGE COMP-3.
