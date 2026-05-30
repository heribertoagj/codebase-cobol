      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : REGISTROS-DETALHE DO  ARQUIVO DE CONFIRMACOES DE    *
      *            DOMICILIO BANCARIO ACRESCIDOS DE DADOS DO HEADER    *
      *  LRECL   : 047 (FB)                                            *
      *  NOME INC: I#BVVEGR                                            *
      *  DATA    : 28/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEGR-REG.
           05  BVVEGR-CNPJCPF-NRO             PIC 9(009)     COMP-3.
           05  BVVEGR-CNPJ-FILIAL             PIC 9(005)     COMP-3.
           05  BVVEGR-CNPJCPF-CTRL            PIC 9(002).
           05  BVVEGR-PONTO-VENDA             PIC 9(009)     COMP-3.
           05  BVVEGR-AGENCIA                 PIC 9(005)     COMP-3.
           05  BVVEGR-CONTA-NRO               PIC 9(013)     COMP-3.
           05  BVVEGR-CONTA-DIG               PIC X(001).
           05  BVVEGR-TIPO-ALTERACAO          PIC 9(001).
           05  BVVEGR-DTSOLIC-ALTERA          PIC 9(009)     COMP-3.
           05  BVVEGR-NRO-SEQUENCIAL          PIC 9(005)     COMP-3.
           05  BVVEGR-DATA-CTRL               PIC 9(009)     COMP-3.
           05  BVVEGR-VERSAO-ARQ              PIC 9(009)     COMP-3.
           05  BVVEGR-BANCO-ENVIO             PIC 9(003)     COMP-3.
      *----------------------------------------------------------------*
      * 001-005 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 006-008 FILIAL DO CNPJ DO ESTABELECIMENTO
      * 009-010 CONTROLE DO CNPJ/CPF DO ESTABELECIMENTO
      * 011-015 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 016-018 AGENCIA DA CONTA CORRENTE DO ESTABELECIMENTO(SEM DIGITO)
      * 019-025 NUMERO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 026-026 DIGITO DA CONTA CORRENTE DO ESTABELECIMENTO
      * 027-027 TIPO DE ALTERACAO(1=GE 2=WF 3=LEGADO)
      * 028-032 DATA DA SOLICITACAO DA ALTERACAO (AAAAMMDD)
      * 033-035 NUMERO SEQUENCIAL DE CONTROLE
      * 036-040 DATA DE ENVIO PARA CONTROLE DE CLASSIFICACAO
      * 041-045 VERSAO DO ARQUIVO
      * 046-047 CODIGO DO BANCO DE ENVIO
