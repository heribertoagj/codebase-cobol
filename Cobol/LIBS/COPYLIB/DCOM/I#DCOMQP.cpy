      ******************************************************************
      * SISTEMA : DCOM - I#DCOMQP                                      *
      * TAMANHO : 0194 BYTES                                           *
      * ARQUIVO : HISTORICO MEIO LIBERACAO                             *
      * INCLUDE DA TABELA DCOMB0B4 - DB2PRD.THIST_MEIO_LIBRC           *
      ******************************************************************
BRQ001* ALTERACAO  -  BRQ DIGITAL SOLUTIONS                            *
BRQ001*----------------------------------------------------------------*
BRQ001* DATA....:  22/11/2018                                          *
BRQ001* OBJETIVO:  INCLUIR INDICADOR DE LIBERACAO NOS CANAIS           *
BRQ001* PROJETO.:  BRQ - 18/0456 - BORDERO ELETRONICO                  *
BRQ001*================================================================*
       01  QP-B4-REGISTRO.
01  03     05 QP-B4-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04  05     05 QP-B4-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09  02     05 QP-B4-CMEIO-LIBRC-DESC     PIC S9(3)V USAGE COMP-3.
11  02     05 QP-B4-NOPER-MEIO-LIBRC     PIC S9(3)V USAGE COMP-3.
13  26     05 QP-B4-HULT-ATULZ           PIC X(26).
39  02     05 QP-B4-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
41  26     05 QP-B4-HSIT-DESC-COML       PIC X(26).
67  02     05 QP-B4-CBCO                 PIC S9(3)V USAGE COMP-3.
69  03     05 QP-B4-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
72  07     05 QP-B4-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
79  02     05 QP-B4-CPOSTO-SERVC         PIC S9(3)V USAGE COMP-3.
81  05     05 QP-B4-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
86  03     05 QP-B4-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
89  02     05 QP-B4-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
91  60     05 QP-B4-IPSSOA-DESC-COML     PIC X(60).
151 06     05 QP-B4-CCLUB                PIC S9(10)V USAGE COMP-3.
157 09     05 QP-B4-VLIBRC-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
166 04     05 QP-B4-PLIBRC-DESC-COML     PIC S9(3)V9(3) USAGE COMP-3.
170 10     05 QP-B4-DLIBRC-DESC-COML     PIC X(10).
180 01     05 QP-B4-CINDCD-LIBRC-ANTCP   PIC X(1).
181 05     05 QP-B4-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
186 08     05 QP-B4-CTERM                PIC X(8).
194 01     05 QP-B4-CINDCD-LIBRC-CANAL   PIC X(1).
