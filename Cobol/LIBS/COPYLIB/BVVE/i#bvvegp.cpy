      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CARGA/BAIXA TAB BVVEB023 - TSIT_PPSTA_PCERO         *
      *  LRECL   : ??? (FB)                                            *
      *  NOME INC: I#BVVEGP                                            *
      *  DATA    : 24/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGP-REG.
           05  BVVEGP-CSIT-AFLIA-PCERO      PIC X(01).
      * 001-001 CODIGO DA SITUACAO DA PROPOSTA DE AFILIACAO NA REDECARD
           05  BVVEGP-ISIT-AFLIA-PCERO      PIC X(60).
      * 002-061 DESCRICAO SITUACAO DA PROPOSTA DE AFILIACAO NA REDECARD
           05  BVVEGP-CACTAC-CORRC-PPSTA    PIC X(01).
      * 062-062 INDICADOR DE ACEITACAO DE CORRECAO NA PROPOSTA
