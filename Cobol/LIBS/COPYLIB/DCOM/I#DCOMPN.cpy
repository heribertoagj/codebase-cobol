      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPN                                      *
      * TAMANHO : 0116 BYTES                                           *
      * ARQUIVO : TRANSITORIA OPERACAO AUTORIZACAO DESC. COM.          *
      * INCLUDE DA TABELA DCOMB0H9 - DB2PRD.TTRASI_OPER_AUTRZ          *
      ******************************************************************
       01  PN-H9-REGISTRO.
 01 03     05 PN-H9-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
 04 05     05 PN-H9-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
 09 03     05 PN-H9-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
 12 02     05 PN-H9-CTPO-AUTRZ-ESPCL     PIC S9(3)V USAGE COMP-3.
 14 26     05 PN-H9-HULT-ATULZ           PIC X(26).
 40 04     05 PN-H9-PMIN-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
 44 04     05 PN-H9-PMAX-TX-JURO-REGRA   PIC S9(3)V9(3) USAGE COMP-3.
 48 05     05 PN-H9-PTX-JURO-OPER-DESC   PIC S9(3)V9(5) USAGE COMP-3.
 53 02     05 PN-H9-CINDCD-FREQ-TX       PIC X(2).
 55 04     05 PN-H9-CCTRO-CUSTO          PIC X(4).
 59 05     05 PN-H9-CAUTRZ-TX-TESOU      PIC S9(9)V USAGE COMP-3.
 64 40     05 PN-H9-IAUTRZ-TX-TESOU      PIC X(40).
104 05     05 PN-H9-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
109 08     05 PN-H9-CTERM                PIC X(8).
