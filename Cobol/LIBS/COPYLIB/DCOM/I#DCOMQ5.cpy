      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQ5                                      *
      * TAMANHO : 0116 BYTES                                           *
      * ARQUIVO : OPERACAO AUTORIZACAO DESCONTO COMERCIAL              *
      * INCLUDE DA TABELA DCOMB0C4 - DB2PRD.TOPER_AUTRZ_DESC           *
      ******************************************************************
       01  Q5-C4-REGISTRO.
 01 03     05 Q5-C4-DANO-OPER-DESC       PIC S9(4)V     USAGE COMP-3.
 04 05     05 Q5-C4-NSEQ-OPER-DESC       PIC S9(9)V     USAGE COMP-3.
 09 03     05 Q5-C4-NPCELA-DESC-COML     PIC S9(5)V     USAGE COMP-3.
 12 02     05 Q5-C4-CTPO-AUTRZ-ESPCL     PIC S9(3)V     USAGE COMP-3.
 14 26     05 Q5-C4-HULT-ATULZ           PIC X(26).
 40 04     05 Q5-C4-PMIN-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
 44 04     05 Q5-C4-PMAX-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
 48 05     05 Q5-C4-PTX-JURO-OPER-DESC   PIC S9(3)V9(5) USAGE COMP-3.
 53 02     05 Q5-C4-CINDCD-FREQ-TX       PIC X(2).
 55 04     05 Q5-C4-CCTRO-CUSTO          PIC X(4).
 59 05     05 Q5-C4-CAUTRZ-TX-TESOU      PIC S9(9)V    USAGE COMP-3.
 64 40     05 Q5-C4-IAUTRZ-TX-TESOU      PIC X(40).
104 05     05 Q5-C4-CFUNC-BDSCO          PIC S9(9)V    USAGE COMP-3.
109 08     05 Q5-C4-CTERM                PIC X(8).
