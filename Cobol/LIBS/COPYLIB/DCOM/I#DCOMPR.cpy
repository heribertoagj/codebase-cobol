      ******************************************************************
      * SISTEMA : DCOM - I#DCOMPR                                      *
      * TAMANHO : 0194 BYTES                                           *
      * ARQUIVO : TRANSITORIA OPERACAO MEIO DE LIBERACAO               *
      * INCLUDE DA TABELA DCOMB0H8 - (TTRASI_MEIO_LIBRC_LIBRC)         *
      ******************************************************************
BRQ001* ALTERACAO  -  BRQ DIGITAL SOLUTIONS                            *
BRQ001*----------------------------------------------------------------*
BRQ001* DATA....:  22/11/2018                                          *
BRQ001* OBJETIVO:  INCLUIR INDICADOR DE LIBERACAO NOS CANAIS           *
BRQ001* PROJETO.:  BRQ - 18/0456 - BORDERO ELETRONICO                  *
BRQ001*================================================================*
       01  PR-H8-REGISTRO.
 01 03     05 PR-H8-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
 04 05     05 PR-H8-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
 09 02     05 PR-H8-CMEIO-LIBRC-DESC     PIC S9(3)V USAGE COMP-3.
 11 02     05 PR-H8-NOPER-MEIO-LIBRC     PIC S9(3)V USAGE COMP-3.
 13 02     05 PR-H8-CSIT-DESC-COML       PIC S9(3)V USAGE COMP-3.
 15 26     05 PR-H8-HSIT-DESC-COML       PIC X(26).
 41 02     05 PR-H8-CBCO                 PIC S9(3)V USAGE COMP-3.
 43 03     05 PR-H8-CAG-BCRIA            PIC S9(5)V USAGE COMP-3.
 46 07     05 PR-H8-CCTA-BCRIA-CLI       PIC S9(13)V USAGE COMP-3.
 53 02     05 PR-H8-CPOSTO-SERVC         PIC S9(3)V USAGE COMP-3.
 55 05     05 PR-H8-CCNPJ-CPF            PIC S9(9)V USAGE COMP-3.
 60 03     05 PR-H8-CFLIAL-CNPJ          PIC S9(5)V USAGE COMP-3.
 63 02     05 PR-H8-CCTRL-CNPJ-CPF       PIC S9(2)V USAGE COMP-3.
 65 60     05 PR-H8-IPSSOA-DESC-COML     PIC X(60).
125 06     05 PR-H8-CCLUB                PIC S9(10)V USAGE COMP-3.
131 09     05 PR-H8-VLIBRC-DESC-COML     PIC S9(15)V9(2) USAGE COMP-3.
140 04     05 PR-H8-PLIBRC-DESC-COML     PIC S9(3)V9(3) USAGE COMP-3.
144 10     05 PR-H8-DLIBRC-DESC-COML     PIC X(10).
154 01     05 PR-H8-CINDCD-LIBRC-ANTCP   PIC X(1).
155 26     05 PR-H8-HULT-ATULZ           PIC X(26).
181 05     05 PR-H8-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
186 08     05 PR-H8-CTERM                PIC X(8).
194 01     05 PR-H8-CINDCD-LIBRC-CANAL   PIC X(1).
