      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQ9                                      *
      * TAMANHO : 0194 BYTES                                           *
      * ARQUIVO : OPERACAO MEIO LIBERACAO                              *
      * INCLUDE DA TABELA DCOMB0C7 - DB2PRD.TOPER_MEIO_LIBRC           *
      ******************************************************************
BRQ001* ALTERACAO  -  BRQ DIGITAL SOLUTIONS                            *
BRQ001*----------------------------------------------------------------*
BRQ001* DATA....:  22/11/2018                                          *
BRQ001* OBJETIVO:  INCLUIR INDICADOR DE LIBERACAO NOS CANAIS           *
BRQ001* PROJETO.:  BRQ - 18/0456 - BORDERO ELETRONICO                  *
BRQ001*================================================================*
       01  Q9-C7-REGISTRO.
01  03     05 Q9-C7-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04  05     05 Q9-C7-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09  02     05 Q9-C7-CMEIO-LIBRC-DESC     PIC S9(3)V USAGE COMP-3.
11  02     05 Q9-C7-NOPER-MEIO-LIBRC     PIC S9(3)V USAGE COMP-3.
13  02     05 Q9-C7-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
15  26     05 Q9-C7-HSIT-DESC-COML       PIC X(26).
41  02     05 Q9-C7-CBCO                 PIC S9(3)V USAGE COMP-3.
43  03     05 Q9-C7-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
46  07     05 Q9-C7-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
53  02     05 Q9-C7-CPOSTO-SERVC         PIC S9(3)V USAGE COMP-3.
55  05     05 Q9-C7-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
60  03     05 Q9-C7-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
63  02     05 Q9-C7-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
65  60     05 Q9-C7-IPSSOA-DESC-COML     PIC X(60).
125 06     05 Q9-C7-CCLUB                PIC S9(10)V USAGE COMP-3.
131 09     05 Q9-C7-VLIBRC-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
140 04     05 Q9-C7-PLIBRC-DESC-COML     PIC S9(3)V9(3) USAGE COMP-3.
144 10     05 Q9-C7-DLIBRC-DESC-COML     PIC X(10).
154 01     05 Q9-C7-CINDCD-LIBRC-ANTCP   PIC X(1).
155 26     05 Q9-C7-HULT-ATULZ           PIC X(26).
181 05     05 Q9-C7-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
186 08     05 Q9-C7-CTERM                PIC X(8).
194 01     05 Q9-C7-CINDCD-LIBRC-CANAL   PIC X(1).
