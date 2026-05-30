      ******************************************************************
      * SISTEMA : DCOM - I#DCOMS6                                      *
      * TAMANHO : 0060 BYTES                                           *
      * ARQUIVO : ESTOQUE OPERACAO ATIVO                               *
      * INCLUDE DA TABELA DCOMB0K5 - DB2PRD.TESTOQ_OPER_ATIVO          *
      ******************************************************************
       01  S6-K5-REGISTRO.
  1  5     05  S6-K5-CCNPJ-CPF            PIC S9(09)V COMP-3.
  6  8     05  S6-K5-CFLIAL-CNPJ          PIC S9(05)V COMP-3.
  9 10     05  S6-K5-CPRODT               PIC S9(03)V COMP-3.
 11 12     05  S6-K5-CSPROD-DESC-COML     PIC S9(03)V COMP-3.
 13 17     05  S6-K5-CCONTR-CONVE-DESC    PIC S9(09)V COMP-3.
 18 19     05  S6-K5-CELMTO-DESC-COML     PIC S9(03)V COMP-3.
 20 21     05  S6-K5-CCTRL-CNPJ-CPF       PIC S9(02)V COMP-3.
 22 26     05  S6-K5-QOPER-ATIVO-ESTOQ    PIC S9(09)V COMP-3.
 27 34     05  S6-K5-VOPER-ATIVO-ESTOQ    PIC S9(13)V9(2) COMP-3.
 35 44     05  S6-K5-DINCL-REG            PIC  X(10).
 45 45     05  S6-K5-DINCL-REG-NULL       PIC  X(01).
 46 50     05  S6-K5-CFUNC-BDSCO          PIC S9(09)V COMP-3.
 51 51     05  S6-K5-CFUNC-BDSCO-NULL     PIC  X(01).
 52 59     05  S6-K5-CTERM                PIC  X(08).
 60 60     05  S6-K5-CTERM-NULL           PIC  X(01).

