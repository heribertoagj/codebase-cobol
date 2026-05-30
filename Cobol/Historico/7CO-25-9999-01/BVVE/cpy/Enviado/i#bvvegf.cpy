      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : CARGA/BAIXA DA TABELA DE ENDERECOS                  *
      *  LRECL   : 132 (FB)                                            *
      *  NOME INC: I#BVVEGF                                            *
      *  DATA    : 18/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGF-REG.
7C2511     05  BVVEGF-CCNPJ-CPF-ESTBL       PIC X(09).
7C2511*    05  BVVEGF-CCNPJ-CPF-ESTBL       PIC 9(09)      COMP-3.
7C2511     05  BVVEGF-CFLIAL-CNPJ-ESTBL     PIC X(04).
7C2511*    05  BVVEGF-CFLIAL-CNPJ-ESTBL     PIC 9(05)      COMP-3.
           05  BVVEGF-CTPO-ENDER-ESTBL      PIC X(01).
           05  BVVEGF-ELOGDR-ESTBL-COML     PIC X(40).
           05  BVVEGF-ELOGDR-NRO-ESTBL      PIC X(05).
           05  BVVEGF-ECOMPL-ESTBL-COML     PIC X(20).
           05  BVVEGF-ECOMPL-ESTBL-COML-N   PIC X(01).
           05  BVVEGF-EBAIRO-ESTBL-COML     PIC X(20).
           05  BVVEGF-ECIDDE-ESTBL-COML     PIC X(30).
           05  BVVEGF-CSGL-UF               PIC X(02).
           05  BVVEGF-CCEP-COMPL            PIC 9(03)      COMP-3.
           05  BVVEGF-CCEP                  PIC 9(05)      COMP-3.
      *----------------------------------------------------------------*
      * 001-005 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 006-008 NUMERO DA FILIAL DO CNPJ DO ESTABELECIMENTO
      * 009-009 TIPO DE ENDERECO (C=COMERCIAL E=CORRESPONDENCIA)
      * 010-049 NOME DO LOGRADOURO (RUA, AV, ALAMEDA, ETC...)
      * 050-054 NUMERO DO IMOVEL NO LOGRADOURO
      * 055-074 COMPLEMENTO DO ENDERECO
      * 075-075 INDICADOR DE NULIDADE DO COMPLEMENTO DO ENDERECO
      * 076-095 NOME DO BAIRRO
      * 096-125 NOME DA CIDADE
      * 126-127 SIGLA DA UF (UNIDADE FEDERATIVA)
      * 128-129 COMPLEMENTO DO CEP
      * 130-132 NUMERO DO CEP
