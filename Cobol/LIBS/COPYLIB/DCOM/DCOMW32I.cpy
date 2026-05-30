      ******************************************************************
      * NOME BOOK : DCOMW32I                                           *
      * DESCRICAO : BOOK DE INTERFACE FEMP3A6C                         *
      * FUNCAO    : SALVAR CAMPOS RELACIONADOS FORMULARIO E DICM       *
      *             RETORNAR PDF EM BINARIO COM AS ALTERACOES          *
      * TIPO      : FUNCIONAL                                          *
      * DATA      : 04/04/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * EMPRESA   : FOURSYS                                            *
      * SISTEMA   : FEMP                                               *
      * TAMANHO   : 20188 BYTES                                        *
      ******************************************************************
      * DCOMW32I-E-CTPO-ACESSO        = DEFINE O COMPORTAMENTO DO PROG.*
      *                                 G - GRAVAR DOCUMENTO           *
      *                                 C - CONSULTAR DOCUMENTO        *
      * DCOMW32I-E-POPUP              = INDICA SE A IMPRESSAO DEVE SER *
      *                                 COM ASS. DO CREDOR             *
      *                                                                *
      * OS CAMPOS ABAIXO DEVEM SER PREENCHIDOS COM S/N, PARA SEREM     *
      * IMPRESSOS                                                      *
      * DCOMW32I-E-IMPR-VIA-NEGOC     = VIA NEGOCIACAO                 *
      * DCOMW32I-E-IMPR-VIA-N-NEGOC   = VIA NAO NEGOCIAVEL             *
      * DCOMW32I-E-IMPR-CONTR         = CONTRATO                       *
      * DCOMW32I-E-IMPR-APOLIC-SEGUR  = APOLICE DE SEGURO              *
      * DCOMW32I-E-IMPR-TERM-ADSAO    = TERMO DE ADESAO                *
      * DCOMW32I-E-IMPR-DECLR         = DECLARACAO                     *
      * DCOMW32I-E-IMPR-AUTRZ         = AUTORIZACAO                    *
      * DCOMW32I-E-IMPR-NOTA-PROMS    = NOTA PROMISSORIA               *
      * DCOMW32I-E-IMPR-FICHA-CTBIL   = FICHA CONTABIL                 *
      *******GERAR RELATORIO NO CMFE************************************
      * DCOMW32I-E-CDPRODT-BDSC       = CODIGO PRODUTO BRADESCO        *
      * DCOMW32I-E-CFAML-CONTR        = CODIGO FAMILIA CONTRATO        *
      * DCOMW32I-E-CCONTR             = CODIGO CONTRATO                *
      * DCOMW32I-E-CSEQ-ATLZ-CONTR    = CODIGO SEQUENCIA ATUALIZ. CONT.*
      * DCOMW32I-E-VERSAO             = VERSAO                         *
      * DCOMW32I-E-RELEASE            = RELEASE                        *
      *******GRAVAR DOCUMENTO*******************************************
      * DCOMW32I-E-CAG-BCRIA          = CODIGO AGENCIA                 *
      * DCOMW32I-E-CODIGO-GCC         = CODIGO GCC                     *
      * DCOMW32I-E-CTPLAT-IMPRE-DOCTO = CODIGO FORMULARIO BRADESCO     *
      * DCOMW32I-E-CSGL-TPLAT-IMPRE   = NOME DO FORMULARIO BRADESCO    *
      * DCOMW32I-E-CPSSOA-JURID-CONTR = CODIGO PESSOA JURIDICA CONTRATO*
      * DCOMW32I-E-CTPO-CONTR-NEGOC   = CODIGO CLASSIF DO CONTRATO NEG *
      * DCOMW32I-E-NSEQ-CONTR-NEGOC   = NUMERO SEQUENCIA CONTRATO NEG  *
      * DCOMW32I-E-DATA-VIGENCIA      = DATA DA VIGENCIA               *
      *******CONSULTAR DOCUMENTO****************************************
      * DCOMW32I-E-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      * DCOMW32I-E-NPAG-TPLAT-IMPRE   = NUM. DA PAGINA DO DOCUMENTO    *
      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
      * FEMPWA6E-E-NSEQ-FORM-IMPR     = NUM. SEQUENCIAL DO FORMULARIO  *
      *                                 PARA PROCESSOS QUE ENVOLVEM    *
      *                                 MULTIPLOS FORMULARIOS          *
      *                                 SE NAO APLICAVEL, ENVIAR ZEROS *
      *******DADOS DE SESSAO********************************************
      * DCOMW32I-E-CUSUAR-SESSAO      = CODIGO DO USUARIO              *
      * DCOMW32I-E-CDEPEN-SESSAO      = CODIGO DA DEPENDENCIA          *
      * DCOMW32I-E-CTERMINAL          = CODIGO DO TERMINAL             *
      * DCOMW32I-E-CTPO-CANAL         = TIPO DO CANAL                  *
      * DCOMW32I-E-CPSSOA-JURID       = CODIGO PESSOA JURIDICA         *
      * DCOMW32I-E-NSEQ-UND-ORGNZ     = NUM. SEQ. UNID ORGANIZACIONAL  *
      * DCOMW32I-E-EMPR-USUAR-TRAB    = EMPRESA USUARIO                *
      * DCOMW32I-E-DEPEND-USUAR-TRAB  = DEPENDENCIA USUARIO            *
      *******CAMPOS DE SAIDA********************************************
      * DCOMW32I-S-QTDE-TOT-PAG       = QTD. TOTAL DE PAG. DO DOC.     *
      * DCOMW32I-S-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      *                                 CONFORME O DICM                *
      * DCOMW32I-S-WTPLAT-IMPRE-DOCTO = PAG. DO DOC. PDF               *
      ******************************************************************
         05 DCOMW32I-HEADER.
            10 DCOMW32I-COD-LAYOUT          PIC X(008) VALUE 'DCOMW32I'.
            10 DCOMW32I-TAM-LAYOUT          PIC 9(005) VALUE 20188.
         05 DCOMW32I-REGISTRO.
            10 DCOMW32I-BLOCO-ENTRADA.

      *************************************************************
               15 DCOMW32I-E-CTPO-ACESSO        PIC X(01).
               15 DCOMW32I-E-POPUP              PIC X(01).
               15 DCOMW32I-E-FORM-IMPR.
                 20 DCOMW32I-E-IMPR-VIA-NEGOC   PIC X(01).
                 20 DCOMW32I-E-IMPR-VIA-N-NEGOC PIC X(01).
                 20 DCOMW32I-E-IMPR-CONTR       PIC X(01).
                 20 DCOMW32I-E-IMPR-APOLIC-SEGUR
                                                PIC X(01).
                 20 DCOMW32I-E-IMPR-TERM-ADSAO  PIC X(01).
                 20 DCOMW32I-E-IMPR-DECLR       PIC X(01).
                 20 DCOMW32I-E-IMPR-AUTRZ       PIC X(01).
                 20 DCOMW32I-E-IMPR-NOTA-PROMS  PIC X(01).
                 20 DCOMW32I-E-IMPR-FICHA-CTBIL PIC X(01).

      *******GERAR RELATORIOS NO DCOM **********************************
               15 DCOMW32I-E-COD-DOCTO          PIC X(10).
               15 DCOMW32I-E-NUM-OPER           PIC 9(13).
               15 DCOMW32I-TIPO-VIA             PIC 9(1).
               15 DCOMW32I-E-VERSAO             PIC 9(08).
               15 DCOMW32I-E-RELEASE            PIC 9(01).

      *******GRAVAR DOCUMENTO NO DOCG***********************************
               15 DCOMW32I-E-CAG-BCRIA          PIC 9(05).
               15 DCOMW32I-E-CODIGO-GCC         PIC 9(08).
               15 DCOMW32I-E-CTPLAT-IMPRE-DOCTO PIC 9(04).
               15 DCOMW32I-E-CSGL-TPLAT-IMPRE   PIC X(05).
               15 DCOMW32I-E-DATA-VIGENCIA      PIC X(10).

      *******CONSULTAR DOCUMENTO NO DOCG********************************
               15 DCOMW32I-E-NPROT-IMPRE-DOCTO  PIC 9(10).
               15 DCOMW32I-E-NPAG-TPLAT-IMPRE   PIC 9(05).

      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
               15 DCOMW32I-E-NSEQ-FORM-IMPR     PIC 9(03).

      *******DADOS DA PARCELAS******************************************
               15 DCOMW32I-PARCELA              PIC 9(05).

      *******DADOS OBTIDOS NA SESSAO*******************************
            10 DCOMW32I-BLOCO-SESSAO.
               15 DCOMW32I-E-CUSUAR-SESSAO      PIC X(09).
               15 DCOMW32I-E-CDEPEN-SESSAO      PIC 9(05).
               15 DCOMW32I-E-CTERMINAL          PIC X(08).
               15 DCOMW32I-E-CTPO-CANAL         PIC 9(03).
               15 DCOMW32I-E-CPSSOA-JURID       PIC 9(10).
               15 DCOMW32I-E-NSEQ-UND-ORGNZ     PIC 9(08).
               15 DCOMW32I-E-EMPR-USUAR-TRAB    PIC 9(10).
               15 DCOMW32I-E-DEPEND-USUAR-TRAB  PIC 9(08).

      *******BLOCO DE SAIDA *******O*******************************
            10 DCOMW32I-BLOCO-SAIDA.
               15 DCOMW32I-S-QTDE-TOT-PAG       PIC 9(05).
               15 DCOMW32I-S-NPROT-IMPRE-DOCTO  PIC 9(10).
               15 DCOMW32I-S-WTPLAT-IMPRE-DOCTO PIC X(20000).
