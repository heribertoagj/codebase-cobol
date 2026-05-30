      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPB                                      *
      * TAMANHO : 0100 BYTES                                           *
      * ARQUIVO : ARQUIVO "PONTEIRO" CHEQUES EXCLUIDOS PELA CUSTODIA   *
      *           (EXCLUIDOS, DIVERGENTES E PRAZO DE ACOLHIMENTO)      *
      ******************************************************************
       01  PB-REGISTRO.
01  03     05  PB-DANO-OPER-DESC       PIC  9(004) COMP-3.
04  05     05  PB-NSEQ-OPER-DESC       PIC  9(009) COMP-3.
09  03     05  PB-NPCELA-DESC-COML     PIC  9(005) COMP-3.
12  02     05  PB-CBCO-CHQ             PIC  9(003) COMP-3.
14  03     05  PB-CAG-BCRIA-CHQ        PIC  9(005) COMP-3.
17  07     05  PB-CCTA-CORR-CHQ        PIC  9(013) COMP-3.
24  04     05  PB-CCHEQ                PIC  9(007) COMP-3.
28  08     05  PB-VCHEQC               PIC  9(013)V99 COMP-3.
36  08     05  PB-VJURO-CHEQUE         PIC  9(013)V99 COMP-3.
44  08     05  PB-VJURO-APROP          PIC  9(013)V99 COMP-3.
52  08     05  PB-VPSENT-CHEQ          PIC  9(013)V99 COMP-3.
60  01     05  PB-TIPO-EXCLUSAO        PIC  9(001).
      *    1 - EXCLUSAO
      *    2 - DIVERGENCIA
      *    3 - PRAZO DE ACOLHIMENTO
      *    7 - EXCLUIDO EM DUPLICIDADE
      *    8 - EXCLUIDO E JA BAIXADO NO DESCONTO (BAIXA AUTOMATICA)
      *    9 - EXCLUIDO E JA BAIXADO NO DESCONTO (BAIXA COMANDADA)
61  03     05  PB-CAG-BCRIA-CLI        PIC  9(005) COMP-3.
64  10     05  PB-DVCTO-PCELA-DESC     PIC  X(010).
74  05     05  PB-CCONTR-CONVE-DESC    PIC  9(009) COMP-3.
79  02     05  PB-CVRSAO-CONTR-CONVE   PIC  9(003) COMP-3.
81  02     05  PB-CELMTO-DESC-COML     PIC  9(003) COMP-3.
83  01     05  PB-LOCAL-OPERACAO       PIC  X(001).
      *    T - TRANSITORIA
      *    O - OFICIAL
84  10     05  PB-DINIC-OPER           PIC  X(010).
94  07     05  PB-FILLER               PIC  X(007).
