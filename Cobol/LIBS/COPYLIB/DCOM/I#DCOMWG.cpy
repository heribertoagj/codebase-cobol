      *----------------------------------------------------------------*
      * SISTEMA = BDCO - DCOM               BOOK     =  I#DCOMWG       *
      * ARQUIVO = AJUSTE MEIO DE LIBERACAO  SEQUENCIAL                 *
      * LRECL   = 250 BYTES                 01-11-2008                 *
      *----------------------------------------------------------------*
       01  WG-REGISTRO.
01  03     05 WG-DANO-OPER-DESC       PIC S9(04)V       COMP-3.
04  05     05 WG-NSEQ-OPER-DESC       PIC S9(09)V       COMP-3.
09  02     05 WG-CMEIO-LIBRC-DESC     PIC S9(03)V       COMP-3.
11  02     05 WG-NOPER-MEIO-LIBRC     PIC S9(03)V       COMP-3.
13  02     05 WG-CSIT-DESC-COML       PIC S9(03)V       COMP-3.
15  26     05 WG-HSIT-DESC-COML       PIC  X(26).
41  02     05 WG-CBCO                 PIC S9(03)V       COMP-3.
43  03     05 WG-CAG-BCRIA            PIC S9(05)V       COMP-3.
46  07     05 WG-CCTA-BCRIA-CLI       PIC S9(13)V       COMP-3.
53  02     05 WG-CPOSTO-SERVC         PIC S9(03)V       COMP-3.
55  05     05 WG-CCNPJ-CPF            PIC S9(09)V       COMP-3.
60  03     05 WG-CFLIAL-CNPJ          PIC S9(05)V       COMP-3.
63  02     05 WG-CCTRL-CNPJ-CPF       PIC S9(02)V       COMP-3.
65  60     05 WG-IPSSOA-DESC-COML     PIC  X(60).
125 06     05 WG-CCLUB                PIC S9(10)V       COMP-3.
131 09     05 WG-VLIBRC-DESC-COML     PIC S9(15)V9(2)   COMP-3.
140 04     05 WG-PLIBRC-DESC-COML     PIC S9(03)V9(3)   COMP-3.
144 10     05 WG-DLIBRC-DESC-COML     PIC  X(10).
154 01     05 WG-CINDCD-LIBRC-ANTCP   PIC  X(01).
155 26     05 WG-HULT-ATULZ           PIC  X(26).
181 05     05 WG-CFUNC-BDSCO          PIC S9(09)V       COMP-3.
186 08     05 WG-CTERM                PIC  X(08).
194 57     05 FILLER                  PIC  X(57).
