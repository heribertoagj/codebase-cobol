      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQQ                                      *
      * TAMANHO : 0116 BYTES                                           *
      * ARQUIVO : HISTORICO OPERACAO AUTORIZACAO DESCONTO              *
      * INCLUDE DA TABELA DCOMB0B5 - DB2PRD.THIST_OPER_AUTRZ           *
      ******************************************************************
       01  QQ-B5-REGISTRO.
 01 03     05 QQ-B5-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
 04 05     05 QQ-B5-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
 09 03     05 QQ-B5-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
 12 02     05 QQ-B5-CTPO-AUTRZ-ESPCL     PIC S9(3)V USAGE COMP-3.
 14 26     05 QQ-B5-HULT-ATULZ           PIC X(26).
 40 04     05 QQ-B5-PMIN-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
 44 04     05 QQ-B5-PMAX-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
 48 05     05 QQ-B5-PTX-JURO-OPER-DESC   PIC S9(3)V9(5) USAGE COMP-3.
 53 02     05 QQ-B5-CINDCD-FREQ-TX       PIC X(2).
 55 04     05 QQ-B5-CCTRO-CUSTO          PIC X(4).
 59 05     05 QQ-B5-CAUTRZ-TX-TESOU      PIC S9(9)V USAGE COMP-3.
 64 40     05 QQ-B5-IAUTRZ-TX-TESOU      PIC X(40).
104 05     05 QQ-B5-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
109 08     05 QQ-B5-CTERM                PIC X(8).
