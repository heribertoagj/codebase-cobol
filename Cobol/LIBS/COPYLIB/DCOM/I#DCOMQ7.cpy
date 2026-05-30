      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQ7                                      *
      * TAMANHO : 0038 BYTES                                           *
      * ARQUIVO : OPERACAO ISENCAO DESCONTO COMERCIAL                  *
      * INCLUDE DA TABELA DCOMB0C6 - DB2PRD.TOPER_ISENC_DESC           *
      ******************************************************************
       01  Q7-C6-REGISTRO.
01 03      05 Q7-C6-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04 05      05 Q7-C6-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09 02      05 Q7-C6-CTPO-ISENC-DESC      PIC S9(3)V USAGE COMP-3.
11 26      05 Q7-C6-HULT-ATULZ           PIC X(26).
37 02      05 Q7-C6-CABRGC-ISENC-DESC    PIC S9(3)V USAGE COMP-3.
