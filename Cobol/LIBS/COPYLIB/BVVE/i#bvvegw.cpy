      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : DADOS DO ARQUIVO DE ENVIO DE SOLICITACOES DE        *
      *            TRANSFERENCIA  DE  DOMICILIO  BANCARIO  PARA        *
      *            ATUALIZACAO DA TABELA E EMISSAO DE RELATORIOS       *
      *  LRECL   : 109 (FB)                                            *
      *  NOME INC: I#BVVEGW                                            *
      *  DATA    : 25/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGW-REG.
           05  BVVEGW-CNPJCPF-NRO             PIC 9(009)     COMP-3.
           05  BVVEGW-CNPJ-FILIAL             PIC 9(005)     COMP-3.
           05  BVVEGW-CNPJCPF-CTRL            PIC 9(002)     COMP-3.
           05  BVVEGW-CPTO-VDA-ESTBL          PIC 9(009)     COMP-3.
           05  BVVEGW-CTPO-OPER-FINCR         PIC X(001).
           05  BVVEGW-CIDTFD-BANDE-ESTBL      PIC X(002).
           05  BVVEGW-DINCL-ESTBL-COML        PIC 9(009)     COMP-3.
           05  BVVEGW-CAG-BCRIA               PIC 9(005)     COMP-3.
           05  BVVEGW-CCTA-BCRIA-CLI          PIC 9(013)     COMP-3.
           05  BVVEGW-DIG-CTA-BCRIA           PIC X(001).
           05  BVVEGW-CBCO-ORIGE-ESTBL        PIC 9(003)     COMP-3.
           05  BVVEGW-CAG-ORIGE-ESTBL         PIC 9(005)     COMP-3.
           05  BVVEGW-CCTA-ORIGE-ESTBL        PIC 9(013)     COMP-3.
           05  BVVEGW-CDIG-CTA-ORIGE          PIC X(001).
           05  BVVEGW-RAZAO-SOCIAL            PIC X(040).
           05  BVVEGW-CDEPDC                  PIC 9(005)     COMP-3.
           05  BVVEGW-CFUNC-BDSCO             PIC 9(009)     COMP-3.
           05  BVVEGW-DATA-ENVIO              PIC 9(009)     COMP-3.
           05  BVVEGW-HORA-ENVIO              PIC 9(007)     COMP-3.
           05  BVVEGW-VERSAO-ARQ              PIC 9(009)     COMP-3.
      *----------------------------------------------------------------*
      * 001-005 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 006-008 FILIAL DO CNPJ DO ESTABELECIMENTO
      * 009-010 CONTROLE DO CNPJ/CPF DO ESTABELECIMENTO
      * 011-015 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 016-016 TIPO OPERACAO FINANCEIRA/PRODUTO (D=DEBITO C=CREDITO)
      * 017-018 SIGLA DO PRODUTO
      *         CR - MASTERCARD CREDITO
      *         DB - MASTERCARD DEBITO
      *         VC - VISA CREDITO
      *         VD - VISA DEBITO
      * 019-023 DATA DA INCLUSAO DO REGISTRO (AAAAMMDD)
      * 024-026 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO
      * 027-033 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 034-034 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 035-036 BANCO DE ORIGEM DO ESTABELECIMENTO
      * 037-039 AGENCIA DE ORIGEM DO ESTABELECIMENTO
      * 040-046 CONTA DE ORIGEM DO ESTABELECIMENTO
      * 047-047 DIGITO DA CONTA DE ORIGEM DO ESTABELECIMENTO
      * 048-087 RAZAO SOCIAL DO ESTABELECIMENTO
      * 088-090 CODIGO DA DEPENDENCIA BRADESCO (AGENCIA/DEPTO)
      * 091-095 CODIGO DO FUNCIONARIO QUE INCLUIU O REGISTRO
      * 096-100 DATA DO ENVIO DO ARQUIVO 'A REDECARD
      * 101-104 HORARIO DO ENVIO DO ARQUIVO 'A REDECARD
      * 105-109 VERSAO DE ARQUIVO ENVIADO 'A REDECARD
      *----------------------------------------------------------------*
