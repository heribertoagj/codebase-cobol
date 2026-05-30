      *----------------------------------------------------------------*
      *  SISTEMA : BVVE - CARTAO DE CREDITO / VENDEDOR                 *
      *  ARQUIVO : DETALHES DO MOVIMENTO DE RETORNO DE AFILIACOES      *
      *            DE ESTABELECIMENTOS ENVIADO PELA REDECARD           *
      *  LRECL   : 183 (FB)                                            *
      *  NOME INC: I#BVVEG7                                            *
      *  DATA    : 30/07/2008                                          *
      *----------------------------------------------------------------*
       01  BVVEG7-REG.
           05  BVVEG7-NRO-LOTE                PIC 9(07)      COMP-3.
           05  BVVEG7-DATA-LOTE               PIC 9(09)      COMP-3.
           05  BVVEG7-HHMMSS-LOTE             PIC 9(07)      COMP-3.
           05  BVVEG7-IDENT-EMISSOR           PIC 9(03)      COMP-3.
           05  BVVEG7-NUMERO-REG              PIC 9(07)      COMP-3.
           05  BVVEG7-CNPJ-ESTAB.
               10  BVVEG7-ECNPJ-NRO           PIC 9(09)      COMP-3.
               10  BVVEG7-ECNPJ-FILIAL        PIC 9(05)      COMP-3.
               10  BVVEG7-ECNPJ-CTRL          PIC 9(02).
           05  BVVEG7-INDIC-FILIACAO          PIC X(01).
           05  BVVEG7-CODIGO-MOTIVO           PIC 9(05)      COMP-3.
           05  BVVEG7-NUMERO-HE               PIC 9(09)      COMP-3.
           05  BVVEG7-ABERTURA-SOLIC          PIC 9(09)      COMP-3.
           05  BVVEG7-DATA-FILIACAO           PIC 9(09)      COMP-3.
           05  BVVEG7-PREVISAO-INSTAL         PIC 9(09)      COMP-3.
           05  BVVEG7-INSTAL-MAQUINETA        PIC 9(09)      COMP-3.
           05  BVVEG7-PREVISAO-TECNO          PIC 9(09)      COMP-3.
           05  BVVEG7-INSTAL-TECNO            PIC 9(09)      COMP-3.
           05  BVVEG7-DESCRICAO-MOTIVO        PIC X(60).
           05  BVVEG7-PONTO-VENDA             PIC 9(09)      COMP-3.
           05  BVVEG7-INDIC-INTERNET          PIC X(001).
           05  BVVEG7-RAZAO-SOCIAL            PIC X(27).
           05  BVVEG7-CODIGO-CANAL            PIC X(02).
           05  BVVEG7-AGENCIA-FILIACAO        PIC X(04).
           05  BVVEG7-CNPJ-VENDEDOR.
               10  BVVEG7-VCNPJ-NRO           PIC 9(09)      COMP-3.
               10  BVVEG7-VCNPJ-FILIAL        PIC 9(05)      COMP-3.
               10  BVVEG7-VCNPJ-CTRL          PIC 9(02).
           05  BVVEG7-TIPO-MOVIMENTO          PIC X(01).
           05  BVVEG7-CODIGO-CELULA           PIC X(05).
      *----------------------------------------------------------------*
      * 001-004 NUMERO DO LOTE
      * 005-009 DATA DE GERACAO DO LOTE
      * 010-013 HORARIO DE GERACAO DO LOTE (HHMMSS)
      * 014-015 IDENTIFICACAO DO EMISSOR (CODIGO DO BANCO)
      * 016-019 NUMERO DO REGISTRO NO ARQUIVO DE RETORNO
      * 020-024 NUMERO DO CNPJ/CPF DO ESTABELECIMENTO
      * 025-027 FILIAL DO CNPJ DO ESTABELECIMENTO
      * 028-029 CONTROLE DO CNPJ/CPF DO ESTABELECIMENTO
      * 030-030 INDICADOR DE FILIACAO (STATUS REDECARD)
      * 031-033 CODIGO DO MOTIVO DO ERRO
      * 034-038 NUMERO DO HE
      * 039-043 DATA DE ABERTURA DA SOLICITACAO DE AFILIACAO
      * 044-048 DATA DE FILIACAO
      * 049-053 DATA DE PREVISAO DE INSTALACAO DA MAQUINETA
      * 054-058 DATA EFETIVA DE INSTALACAO DA MAQUINETA
      * 059-063 DATA PREVISTA PARA DISPONIBILIZACAO DA TECNOLOGIA
      * 064-068 DATA EFETIVA DE DISPONIBILIZACAO DA TECNOLOGIA
      * 069-128 DESCRICAO DO MOTIVO DO ERRO
      * 129-133 NUMERO DO PONTO DE VENDA DO ESTABELECIMENTO
      * 134-134 INDICATIVO DE PROPOSTA GERADA PELA INTERNET
      * 135-161 RAZAO SOCIAL DO ESTABELECIMENTO
      * 162-163 CODIGO DO CANAL
      * 164-167 CODIGO DA AGENCIA DE FILIACAO
      * 168-172 NUMERO DO CNPJ/CPF DA EMPRESA CONTRATADA/VENDEDOR
      * 173-175 FILIAL DO CNPJ DA EMPRESA CONTRATADA
      * 176-177 CONTROLE DO CNPJ/CPF DA EMPRESA CONTRATADA/VENDEDOR
      * 178-178 TIPO DE MOVIMENTO (S=SERVICO E=EMISSOR)
      * 179-183 CODIGO DA CELULA
      *----------------------------------------------------------------*
