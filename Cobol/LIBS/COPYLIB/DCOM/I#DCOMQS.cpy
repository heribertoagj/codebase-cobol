      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQS                                      *
      * TAMANHO : 0038 BYTES                                           *
      * ARQUIVO : HISTORICO OPERACAO ISENCAO DESCONTO                  *
      * INCLUDE DA TABELA DCOMB0B7 - DB2PRD.THIST_OPER_ISENC           *
      ******************************************************************
       01  QS-B7-REGISTRO.
01 03      05 QS-B7-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 QS-B7-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02      05 QS-B7-CTPO-ISENC-DESC      PIC S9(3)V USAGE COMP-3.
11 26      05 QS-B7-HULT-ATULZ           PIC X(26).
37 02      05 QS-B7-CABRGC-ISENC-DESC    PIC S9(3)V USAGE COMP-3.
