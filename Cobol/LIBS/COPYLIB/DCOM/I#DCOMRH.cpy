      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRH                                      *
      * TAMANHO : 0100 BYTES                                           *
      * ARQUIVO : ARQUIVO COM DADOS DA I.O.F. RECOLHIDA SOBRE OS       *
      *           TITULOS REGISTRADOS EM DESCONTO.                     *
      ******************************************************************
       01  RH-REGISTRO.
01  03     05  RH-DANO-OPER-DESC       PIC  9(004) COMP-3.
04  05     05  RH-NSEQ-OPER-DESC       PIC  9(009) COMP-3.
09  03     05  RH-NPCELA-DESC-COML     PIC  9(005) COMP-3.
12  02     05  RH-CSIT-DESC-COML       PIC  9(003) COMP-3.
14  02     05  RH-CPRODT               PIC  9(003) COMP-3.
16  02     05  RH-CSPROD-DESC-COML     PIC  9(003) COMP-3.
18  03     05  RH-CAG-BCRIA            PIC  9(005) COMP-3.
21  07     05  RH-CCTA-BCRIA-CLI       PIC  9(013) COMP-3.
28  01     05  RH-CINDCD-TPO-PSSOA     PIC  X(001).
29  10     05  RH-DREFT-MOVTO-DESC     PIC  X(010).
39  10     05  RH-DVCTO-PCELA-DESC     PIC  X(010).
49  10     05  RH-DPGTO-PCELA-DESC     PIC  X(010).
59  09     05  RH-VPRINC-PCELA-DESC    PIC  9(015)V99 COMP-3.
68  09     05  RH-VJURO-PCELA-DESC     PIC  9(015)V99 COMP-3.
77  09     05  RH-VIOF-PCELA-DESC      PIC  9(015)V99 COMP-3.
86  02     05  RH-TIPO-COBR-IOF        PIC  9(002).
      *        01 - DIGITACAO NO POLO
      *        02 - INCLUSAO DA OPERACAO
      *        03 - ADITAMENTO OPERACAO/PARCELAS
      *        04 - PRORROGACAO DE PARCELAS
      *        05 - BAIXAS COMANDADAS
      *        06 - BAIXAS COBRANCA / SOB CONSULTA
88  13     05  RH-FILLER               PIC  X(013).
