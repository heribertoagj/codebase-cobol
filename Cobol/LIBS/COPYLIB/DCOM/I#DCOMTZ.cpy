      ******************************************************************
      * SISTEMA : DCOM - I#DCOMTZ                                      *
      * TAMANHO : 087 BYTES                                            *
      * ARQUIVO : ARQUIVO DE BAIXA DE PARCELAS (PROJECAO 24 MESES -    *
      *                                         VINCENDOS)             *
      ******************************************************************
       01  TZ-REGISTRO.
01  08     05  TZ-DEVNTO               PIC  9(008).
09  05     05  TZ-CAG-CDENT            PIC  9(005).
14  05     05  TZ-CPOSTO-SERVC         PIC  9(005).
19  02     05  TZ-TPOSTO-SERVC         PIC  X(002).
21  03     05  TZ-CSGMTO-CLI           PIC  9(003).
24  03     05  TZ-CPRODT               PIC  9(003).
27  03     05  TZ-CSPROD-DESC-COML     PIC  9(003).
30  02     05  TZ-CTPO-CONTR-CONVE     PIC  9(002).
      *        01=CTR.LIMITE
      *        02=CONV.CLIENTE
      *        03=CONV.GERAL
32  09     05  TZ-CCONTR-CONVE-DESC PIC  9(009).
41  03     05  TZ-CVRSAO-CONTR-CONVE   PIC  9(003).
44  17     05  TZ-VBAIXA               PIC S9(015)V9(002).
61  09     05  TZ-QBAIXA               PIC  9(009).
70  04     05  TZ-DANO-OPER-DESC       PIC  9(004).
74  09     05  TZ-NSEQ-OPER-DESC       PIC  9(009).
83  05     05  TZ-NPCELA-DESC          PIC  9(005).
