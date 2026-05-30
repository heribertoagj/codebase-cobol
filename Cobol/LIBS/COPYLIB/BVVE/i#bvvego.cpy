      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CARGA/BAIXA TAB BVVEB012 - SIT_PPSTA_AFLIA          *
      *  LRECL   : 033 (FB)                                            *
      *  NOME INC: I#BVVEGO                                            *
      *  DATA    : 24/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGO-REG.
           05  BVVEGO-CSIT-PPSTA-AFLIA      PIC 9(02)      COMP-3.
      * 001-002 CODIGO DA SITUACAO DA PROPOSTA DE AFILIACAO NO BVVE
           05  BVVEGO-RSIT-PPSTA-AFLIA      PIC X(20).
      * 003-022 DESCRICAO DA SITUACAO DA PROPOSTA DE AFILIACAO NO BVVE
           05  BVVEGO-DDSATV-SIT-PPSTA      PIC X(10).
      * 023-032 DATA DESATIVACAO DA SITUACAO DA PROPOSTA DE AFILIACAO
           05  BVVEGO-DDSATV-SIT-PPSTA-N    PIC X(01).
      * 033-033 INDICADOR DE NULIDADE DA DATA DE DESATIVACAO
