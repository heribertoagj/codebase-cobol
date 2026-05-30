      ******************************************************************
      * SISTEMA : DCOM - I#DCOMS7                                      *
      * TAMANHO : 0172 BYTES                                           *
      * ARQUIVO : CHEQUES DESPREZADOS                                  *
      * INCLUDE DA TABELA DCOMB0K6 - DB2PRD.TDESPR_CHEQC_DESC          *
      ******************************************************************
       01  S7-K6-REGISTRO.
 01 03     05  S7-K6-DANO-OPER-DESC            PIC S9(4)V COMP-3.
 04 05     05  S7-K6-NSEQ-OPER-DESC            PIC S9(9)V COMP-3.
 09 02     05  S7-K6-CBCO                      PIC S9(3)V COMP-3.
 11 03     05  S7-K6-CAG-BCRIA                 PIC S9(5)V COMP-3.
 14 06     05  S7-K6-CCTA-CORR-BCRIA           PIC S9(11)V COMP-3.
 20 04     05  S7-K6-CCHEQ                     PIC S9(7)V COMP-3.
 24 02     05  S7-K6-CBCO-CLI-CHEQC            PIC S9(3)V COMP-3.
 26 01     05  S7-K6-CBCO-CLI-CHEQC-NULL       PIC  X(1).
 27 03     05  S7-K6-CDEPDC                    PIC S9(5)V COMP-3.
 30 07     05  S7-K6-CCTA-BCRIA-CLI            PIC S9(13)V COMP-3.
 37 03     05  S7-K6-CFLIAL-EMPR-CHEQC         PIC S9(5)V COMP-3.
 40 04     05  S7-K6-CPROT-LOTE-CHEQC          PIC S9(7)V COMP-3.
 44 10     05  S7-K6-DMOVTO                    PIC X(10).
 54 04     05  S7-K6-CLOTE-CHEQC               PIC S9(7)V COMP-3.
 58 10     05  S7-K6-DVCTO-CHEQ-CTDIA          PIC X(10).
 68 08     05  S7-K6-VCHEQC                    PIC S9(13)V9(2) COMP-3.
 76 01     05  S7-K6-CINDCD-CHEQ-SELEC         PIC X(1).
 77 01     05  S7-K6-CTPO-LOTE-OPER            PIC X(1).
 78 03     05  S7-K6-NPCELA-DESC-COML          PIC S9(5)V COMP-3.
 81 20     05  S7-K6-RANLSE-CREDT-CLI          PIC X(20).
101 10     05  S7-K6-DSOLTC-ANLSE-CREDT        PIC X(10).
111 02     05  S7-K6-CSIT-ANLSE-CREDT          PIC S9(3)V COMP-3.
113 01     05  S7-K6-CINDCD-ISENC-ANLSE        PIC X(1).
114 26     05  S7-K6-HULT-ATULZ                PIC X(26).
140 05     05  S7-K6-CFUNC-BDSCO               PIC S9(9)V COMP-3.
145 08     05  S7-K6-CTERM                     PIC X(8).
153 10     05  S7-K6-DINCL-REG                 PIC X(10).
163 05     05  S7-K6-CCNPJ-CPF-EMITT           PIC S9(9)V COMP-3.
168 03     05  S7-K6-CFLIAL-CNPJ-EMITT         PIC S9(5)V COMP-3.
171 02     05  S7-K6-CCTRL-CNPJ-EMITT          PIC S9(2)V COMP-3.

