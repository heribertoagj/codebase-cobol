      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CARGA/BAIXA DA TABELA DE PROPRIETARIOS DE ESTAB.    *
      *  LRECL   : 059 (FB)                                            *
      *  NOME INC: I#BVVEGG                                            *
      *  DATA    : 18/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGG-REG.
           05  BVVEGG-CCNPJ-CPF-ESTBL       PIC 9(09)      COMP-3.
           05  BVVEGG-CFLIAL-CNPJ-ESTBL     PIC 9(05)      COMP-3.
           05  BVVEGG-CCPF-PPRIE            PIC 9(09)      COMP-3.
           05  BVVEGG-CCTRL-CPF-PPRIE       PIC 9(02)      COMP-3.
           05  BVVEGG-IPPRIE-ESTBL-AFLIA    PIC X(32).
           05  BVVEGG-DNASC-PPRIE           PIC X(10).
           05  BVVEGG-PPRTCP-ACAO-PPRIE     PIC 9(01)V9(2) COMP-3.
      *----------------------------------------------------------------*
      * 001-005 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 006-008 NUMERO DA FILIAL DO CNPJ DO ESTABELECIMENTO
      * 009-013 NUMERO DO CPF DO PROPRIETARIO
      * 016-047 NOME DO PROPRIETARIO
      * 048-057 DATA DE NASCIMENTO DO PROPRIETARIO
      * 058-059 PERCENTUAL DE PARTICIPACAO ACIONARIA
