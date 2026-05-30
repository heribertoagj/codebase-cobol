      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQG                                      *
      * TAMANHO : 0350 BYTES                                           *
      * ARQUIVO : WORKFLOW OPER.DESCONTO COMERCIAL                     *
      * INCLUDE DA TABELA DCOMB0G9- - DB2PRD.TWORKF_OPER_DESC          *
      ******************************************************************
       01  QG-G9-REGISTRO.
01  03     05 QG-G9-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04  05     05 QG-G9-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09  03     05 QG-G9-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
12  02     05 QG-G9-CEVNTO-DESC-COML     PIC S9(3)V USAGE COMP-3.
14  26     05 QG-G9-HWORKF-OPER-DESC     PIC X(26).
40  02     05 QG-G9-CSIT-INIC-DESC       PIC S9(3)V USAGE COMP-3.
42  26     05 QG-G9-HSIT-INIC-DESC       PIC X(26).
68  02     05 QG-G9-CSIT-FNAL-DESC       PIC S9(3)V USAGE COMP-3.
70  26     05 QG-G9-HSIT-FNAL-DESC       PIC X(26).
           05 QG-G9-RWORKF-OPER-DESC.
96  02        49 RWORKF-OPER-DESC-LEN    PIC S9(4) USAGE COMP.
98 240        49 RWORKF-OPER-DESC-TEXT   PIC X(240).
338 05     05 QG-G9-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
343 08     05 QG-G9-CTERM                PIC X(8).
