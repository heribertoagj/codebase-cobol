      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : SOLICITACOES DE TRANSFERENCIA DE DOMICILIO          *
      *            BANCARIO COM RAZAO SOCIAL DO CLIENTE                *
      *  LRECL   : 099 (FB)                                            *
      *  NOME INC: I#BVVEGT                                            *
      *  DATA    : 25/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGT-REG.
           05  BVVEGT-CCNPJ-CPF-ESTBL       PIC 9(09)      COMP-3.
           05  BVVEGT-CFLIAL-CNPJ-ESTBL     PIC 9(05)      COMP-3.
           05  BVVEGT-CPTO-VDA-ESTBL        PIC 9(09)      COMP-3.
           05  BVVEGT-CTPO-OPER-FINCR       PIC X(01).
           05  BVVEGT-CIDTFD-BANDE-ESTBL    PIC X(02).
           05  BVVEGT-DINCL-ESTBL-COML      PIC 9(09)      COMP-3.
           05  BVVEGT-CCTRL-CNPJ-CPF        PIC 9(02)      COMP-3.
           05  BVVEGT-CAG-BCRIA             PIC 9(05)      COMP-3.
           05  BVVEGT-CCTA-BCRIA-CLI        PIC 9(13)      COMP-3.
           05  BVVEGT-CBCO-ORIGE-ESTBL      PIC 9(03)      COMP-3.
           05  BVVEGT-CAG-ORIGE-ESTBL       PIC 9(05)      COMP-3.
           05  BVVEGT-CCTA-ORIGE-ESTBL      PIC 9(13)      COMP-3.
           05  BVVEGT-CDIG-CTA-ORIGE        PIC X(01).
           05  BVVEGT-CDEPDC                PIC 9(05)      COMP-3.
           05  BVVEGT-CFUNC-BDSCO           PIC 9(09)      COMP-3.
           05  BVVEGT-CSIT-SOLTC-PCERO      PIC X(01).
           05  BVVEGT-RAZAO-SOCIAL          PIC X(40).
           05  BVVEGT-CTRAVA-DOMCL-ESTBL    PIC X(01).
           05  BVVEGT-CEMPR-TERC-CONTT      PIC 9(04)      COMP-3.
      *----------------------------------------------------------------*
      * 001-005 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 006-008 FILIAL DO CNPJ DO ESTABELECIMENTO
      * 009-013 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 014-014 TIPO OPERACAO FINANCEIRA/PRODUTO (D=DEBITO C=CREDITO)
      * 015-016 SIGLA DO PRODUTO
      *         CR - MASTERCARD CREDITO
      *         DB - MASTERCARD DEBITO
      *         VC - VISA CREDITO
      *         VD - VISA DEBITO
      * 017-021 DATA DA INCLUSAO DO REGISTRO
      * 022-023 CONTROLE DO CNPJ/CPF DO ESTABELECIMENTO
      * 024-026 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO
      * 026-033 CONTA CORRENTE DO ESTABELECIMENTO
      * 034-035 BANCO DE ORIGEM DO ESTABELECIMENTO
      * 036-038 AGENCIA DE ORIGEM DO ESTABELECIMENTO
      * 039-045 CONTA DE ORIGEM DO ESTABELECIMENTO
      * 046-046 DIGITO DA CONTA DE ORIGEM DO ESTABELECIMENTO
      * 047-049 CODIGO DA DEPENDENCIA BRADESCO (AGENCIA/DEPTO)
      * 050-054 CODIGO DO FUNCIONARIO QUE INCLUIU O REGISTRO
      * 055-055 SIT DA SOLICITACAO (P=PEND E=ENVIADA I=INCONS A=ACATADA)
      * 056-095 RAZAO SOCIAL DO ESTABELECIMENTO
      * 096-096 INDICADOR DE TRAVA (T=TRAVADO D=DESTRAVADO)
      * 097-099 CODIGO DA EMPRESA CONTRATADA
      *----------------------------------------------------------------*
