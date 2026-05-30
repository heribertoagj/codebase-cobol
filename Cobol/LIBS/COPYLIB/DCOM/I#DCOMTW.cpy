      ******************************************************************
      * SISTEMA : DCOM - I#DCOMTW                                      *
      * TAMANHO : 061 BYTES                                            *
      * ARQUIVO : ARQUIVO DE TITULOS DESCONTADOS                       *
      ******************************************************************
       01  TW-REGISTRO.
01  05     05  TW-NUMERO-CLIENTE       PIC  9(009)       COMP-3.
06  03     05  TW-FILIAL-CLIENTE       PIC  9(004)       COMP-3.
09  02     05  TW-CONTR-CLIENTE        PIC  9(002)       COMP-3.
11  01     05  TW-TIPO-CLIENTE         PIC  9(001).
12  09     05  TW-VTOT-TITDESC         PIC  9(015)V9(02) COMP-3.
21  09     05  TW-VTOT-TITDESC-LIQ     PIC  9(015)V9(02) COMP-3.
30  03     05  TW-DANO-OPER-DESC       PIC  9(004)       COMP-3.
33  05     05  TW-NSEQ-OPER-DESC       PIC  9(009)       COMP-3.
38  03     05  TW-NPCELA-DESC-COML     PIC  9(005)       COMP-3.
41  01     05  TW-CTPO-BAIXA-PCELA     PIC  X(001).
42  10     05  TW-DEFETV-BAIXA-PCELA   PIC  X(010).
52  10     05  TW-DVCTO                PIC  X(010).
