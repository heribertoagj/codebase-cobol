      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CARGA/BAIXA DA TABELA DE RAMOS DE ATIVIDADE         *
      *  LRECL   : 112 (FB)                                            *
      *  NOME INC: I#BVVEGB                                            *
      *  DATA    : 10/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGB-REG.
           05  BVVEGB-CRAMO-ATVDD-ESTBL     PIC 9(05)      COMP-3.
           05  BVVEGB-IRAMO-ATVDD-ESTBL     PIC X(60).
           05  BVVEGB-PTX-PCELD-JURO        PIC 9(03)V9(2) COMP-3.
           05  BVVEGB-PTX-CREDT-ROTVO       PIC 9(03)V9(2) COMP-3.
           05  BVVEGB-PTX-CREDT-PCELD       PIC 9(03)V9(2) COMP-3.
           05  BVVEGB-PTX-VDA-INTL          PIC 9(03)V9(2) COMP-3.
           05  BVVEGB-PTX-CATAO-DEB         PIC 9(03)V9(2) COMP-3.
           05  BVVEGB-COBRIG-CERTF-PROFS    PIC X(01).
           05  BVVEGB-CACTAC-PSSOA-FIS      PIC X(01).
           05  BVVEGB-DINCL-ESTBL-COML      PIC X(10).
           05  BVVEGB-CFUNC-BDSCO           PIC 9(09)      COMP-3.
           05  BVVEGB-DALT-ESTBL-COML       PIC X(10).
           05  BVVEGB-DALT-ESTBL-COML-N     PIC X(01).
           05  BVVEGB-CFUNC-ALT-ESTBL       PIC 9(09)      COMP-3.
           05  BVVEGB-CFUNC-ALT-ESTBL-N     PIC X(01).
      *----------------------------------------------------------------*
      * 001-003 CODIGO DO RAMO DE ATIVIDADE
      * 004-063 NOME DO RAMO DE ATIVIDADE
      * 064-066 TAXA DE CREDITO PARA VENDA PARCELADA COM JUROS
      * 067-069 TAXA CREDITO VENDA COM UTILIZACAO DE CREDITO ROTATIVO
      * 070-072 TAXA DE CREDITO PARA VENDA PARCELADA SEM JUROS
      * 073-075 TAXA DE CREDITO PARA VENDA INTERNACIONAL
      * 076-078 TAXA DE CREDITO PARA UTILIZACAO DE CARTAO DE DEBITO
      * 079-079 INDICADOR DE OBRIGATORIEDADE CRM/CRO (S/N)
      * 080-080 INDICADOR ACEITACAO AFILIACAO DE PESSOA FISICA(S/N)
      * 081-090 DATA DE INCLUSAO DO REGISTRO
      * 091-095 CODIGO DO FUNCIONARIO QUE INCLUIU O REGISTRO
      * 096-105 DATA DA ULTIMA ALTERACAO DO REGISTRO
      * 106-106 INDICADOR DE NULIDADE DA DATA DA ULTIMA ALTERACAO
      * 107-111 CODIGO DO FUNCIONARIO QUE ALTEROU O REGISTRO
      * 112-112 INDICADOR DE NULIDADE DO FUNCIONARIO QUE ALTEROU
      *----------------------------------------------------------------*
