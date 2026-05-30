      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CARGA/BAIXA TAB BVVEB024 - TSIT_PPSTA_ESTBL         *
      *  LRECL   : 003 (FB)                                            *
      *  NOME INC: I#BVVEGN                                            *
      *  DATA    : 24/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGN-REG.
           05  BVVEGN-CSIT-AFLIA-PCERO      PIC X(01).
      * 001-001 SITUACAO DA PROPOSTA DE AFILIACAO NA REDECARD
           05  BVVEGN-CSIT-PPSTA-AFLIA      PIC 9(02)      COMP-3.
      * 002-003 SITUACAO DA PROPOSTA DE AFILIACAO NO BVVE
