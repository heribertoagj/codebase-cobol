      ******************************************************************
      * SISTEMA : DCOM - I#DCOMS5                                      *
BSI001* TAMANHO : 0166 BYTES                                           *
      * ARQUIVO : HISTORICO JUSTIFICATIVA DE BAIXAS                    *
      * INCLUDE DA TABELA DCOMB0H3 - DB2PRD.THIST_JUSTF_BAIXA          *
      ******************************************************************
BSI001*                     BSI TECNOLOGIA - ALTERACAO                 *
BSI001*----------------------------------------------------------------*
BSI001* DATA....:  25/07/2016                                          *
BSI001* OBJETIVO:  INCLUIR TRATAMENTO PARA BAIXA POR PREJUIZO.         *
BSI001* PROJETO.:  BSI - 16-0116-01 - BAIXA AMIGAVEL E JUDICIAL.       *
BSI001******************************************************************
       01  S5-H3-REGISTRO.
01  03     05 S5-H3-DANO-OPER-DESC       PIC S9(4)V USAGE COMP-3.
04  05     05 S5-H3-NSEQ-OPER-DESC       PIC S9(9)V USAGE COMP-3.
09  03     05 S5-H3-NPCELA-DESC-COML     PIC S9(5)V USAGE COMP-3.
12  26     05 S5-H3-HBAIXA-PCELA-DESC    PIC X(26).
38  02     05 S5-H3-NLIN-JUSTF-BAIXA     PIC S9(3)V USAGE COMP-3.
40  26     05 S5-H3-HULT-ATULZ           PIC X(26).
66  79     05 S5-H3-RJUSTF-BAIXA-PCELA   PIC X(79).
145 05     05 S5-H3-CFUNC-BDSCO          PIC S9(9)V USAGE COMP-3.
150 08     05 S5-H3-CTERM                PIC X(8).
158 02     05 S5-H3-CMOTVO-BAIXA-PREJ    PIC S9(3)V COMP-3.
160 02     05 S5-H3-CREGRA-CTBIL-PREJ    PIC S9(3)V COMP-3.
162 05     05 S5-H3-CFUNC-BDSCO-AUTRZ    PIC S9(9)V COMP-3.
