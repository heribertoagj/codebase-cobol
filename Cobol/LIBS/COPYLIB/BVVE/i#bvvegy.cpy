      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : DADOS DO RETORNO DE SOLICITACOES DE TRANSFERENCIA   *
      *            DE DOMICILIO BANCARIO  PARA ATUALIZACAO DA TABELA   *
      *            E EMISSAO DE RELATORIOS                             *
      *  LRECL   : 180 (FB)                                            *
      *  NOME INC: I#BVVEGY                                            *
      *  DATA    : 27/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGY-REG.
           05  BVVEGY-CCNPJ-CPF-ESTBL         PIC 9(09)      COMP-3.
           05  BVVEGY-CFLIAL-CNPJ-ESTBL       PIC 9(05)      COMP-3.
           05  BVVEGY-CPTO-VDA-ESTBL          PIC 9(09)      COMP-3.
           05  BVVEGY-CTPO-OPER-FINCR         PIC X(01).
           05  BVVEGY-CIDTFD-BANDE-ESTBL      PIC X(02).
           05  BVVEGY-DINCL-ESTBL-COML        PIC 9(09)      COMP-3.
           05  BVVEGY-CCTRL-CNPJ-CPF          PIC 9(02)      COMP-3.
           05  BVVEGY-CAG-BCRIA               PIC 9(05)      COMP-3.
           05  BVVEGY-CCTA-BCRIA-CLI          PIC 9(13)      COMP-3.
           05  BVVEGY-DIG-CTA-BCRIA           PIC X(01).
           05  BVVEGY-CBCO-ORIGE-ESTBL        PIC 9(03)      COMP-3.
           05  BVVEGY-CAG-ORIGE-ESTBL         PIC 9(05)      COMP-3.
           05  BVVEGY-CCTA-ORIGE-ESTBL        PIC 9(13)      COMP-3.
           05  BVVEGY-CDIG-CTA-ORIGE          PIC X(01).
           05  BVVEGY-CDEPDC                  PIC 9(05)      COMP-3.
           05  BVVEGY-CFUNC-BDSCO             PIC 9(09)      COMP-3.
           05  BVVEGY-DTENVIO-BANCO           PIC 9(09)      COMP-3.
           05  BVVEGY-HRENVIO-BANCO           PIC 9(07)      COMP-3.
           05  BVVEGY-BANCO-RETORNO           PIC 9(03)      COMP-3.
           05  BVVEGY-VERSAO-REDECARD         PIC 9(09)      COMP-3.
           05  BVVEGY-DTENVIO-REDECARD        PIC 9(09)      COMP-3.
           05  BVVEGY-HRENVIO-REDECARD        PIC 9(07)      COMP-3.
           05  BVVEGY-RAZAO-SOCIAL            PIC X(28).
           05  BVVEGY-IND-TRAVA               PIC X(01).
           05  BVVEGY-PV-CENTRALIZ            PIC 9(09)      COMP-3.
           05  BVVEGY-COD-RETORNO             PIC 9(05)      COMP-3.
           05  BVVEGY-MSG-ERRO                PIC X(60).
           05  BVVEGY-ORIGEM-TRAG             PIC 9(04)      COMP-3.
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
      * 027-033 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 034-035 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 035-036 BANCO DE ORIGEM DO ESTABELECIMENTO
      * 037-039 AGENCIA DE ORIGEM DO ESTABELECIMENTO
      * 040-046 CONTA DE ORIGEM DO ESTABELECIMENTO
      * 047-047 DIGITO DA CONTA DE ORIGEM DO ESTABELECIMENTO
      * 048-050 CODIGO DA DEPENDENCIA BRADESCO (AGENCIA/DEPTO)
      * 051-055 CODIGO DO FUNCIONARIO QUE INCLUIU O REGISTRO
      * 056-060 DATA DO ENVIO DO ARQUIVO PELO BANCO
      * 061-064 HORARIO DO ENVIO DO ARQUIVO PELO BANCO
      * 065-066 CODIGO DO BANCO NO ARQUIVO DE RETORNO
      * 067-071 VERSAO DO ARQUIVO DE RETORNO DA REDECARD
      * 072-076 DATA DE ENVIO DO ARQUIVO DE RETORNO PELA REDECARD
      * 077-080 HORARIO DE ENVIO DO ARQUIVO DE RETORNO PELA REDECARD
      * 081-108 RAZAO SOCIAL DO ESTABELECIMENTO
      * 109-109 INDICADOR DE TRAVA DE DOMICILIO (T=TRAVADO D=DESTRAVADO)
      * 110-114 PONTO DE VENDA CENTRALIZADOR DE CREDITOS
      * 115-116 CODIGO DE RETORNO/ERRO
      * 117-177 MENSAGEM DE RETORNO/ERRO
      * 178-180 ORIGEM-TRAG CAMPO CEMPR-TERC-CONTT DA TABELA
      *----------------------------------------------------------------*
