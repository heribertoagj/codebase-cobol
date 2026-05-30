      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQV                                      *
      * TAMANHO : 0350 BYTES                                           *
      * ARQUIVO : HISTORICO WORKFLOW OPER.DESCONTO COMERCIAL           *
      * INCLUDE DA TABELA DCOMB0C1 - THIST_WORKF_OPER                  *
      ******************************************************************
       01  QV-C1-REGISTRO.
01  03     05 QV-C1-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04  05     05 QV-C1-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09  03     05 QV-C1-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
12  02     05 QV-C1-CEVNTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
14  26     05 QV-C1-HWORKF-OPER-DESC     PIC X(26).
40  02     05 QV-C1-CSIT-INIC-DESC       PIC S9(3)V USAGE COMP-3.
42  26     05 QV-C1-HSIT-INIC-DESC       PIC X(26).
68  02     05 QV-C1-CSIT-FNAL-DESC       PIC S9(3)V USAGE COMP-3.
70  26     05 QV-C1-HSIT-FNAL-DESC       PIC X(26).
           05 QV-C1-RWORKF-OPER-DESC.
96  02        49 RWORKF-OPER-DESC-LEN    PIC S9(4) USAGE COMP.
98 240        49 RWORKF-OPER-DESC-TEXT   PIC X(240).
338 05     05 QV-C1-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
343 08     05 QV-C1-CTERM                PIC X(8).
