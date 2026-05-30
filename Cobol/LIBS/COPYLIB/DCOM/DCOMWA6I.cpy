      ******************************************************************
      * NOME BOOK : DCOMWA6I                                           *
      * DESCRICAO : BOOK DE INTERFACE FEMP3A6C                         *
      * FUNCAO    : SALVAR CAMPOS RELACIONADOS FORMULARIO E DICM       *
      *             RETORNAR PDF EM BINARIO COM AS ALTERACOES          *
      * TIPO      : FUNCIONAL                                          *
      * DATA      : 10/08/2018                                         *
      * AUTOR     : VINICIUS TITO                                      *
      * EMPRESA   : CAPGEMINI                                          *
      * SISTEMA   : FEMP                                               *
      * TAMANHO   : 21436 BYTES                                        *
      ******************************************************************
      * DCOMWA6I-E-CTPO-ACESSO        = DEFINE O COMPORTAMENTO DO PROG.*
      *                                 G - GRAVAR DOCUMENTO           *
      *                                 C - CONSULTAR DOCUMENTO        *
      * DCOMWA6I-E-POPUP              = INDICA SE A IMPRESSAO DEVE SER *
      *                                 COM ASS. DO CREDOR             *
      *                                                                *
      * OS CAMPOS ABAIXO DEVEM SER PREENCHIDOS COM S/N, PARA SEREM     *
      * IMPRESSOS                                                      *
      * DCOMWA6I-E-IMPR-VIA-NEGOC     = VIA NEGOCIACAO                 *
      * DCOMWA6I-E-IMPR-VIA-N-NEGOC   = VIA NAO NEGOCIAVEL             *
      * DCOMWA6I-E-IMPR-CONTR         = CONTRATO                       *
      * DCOMWA6I-E-IMPR-APOLIC-SEGUR  = APOLICE DE SEGURO              *
      * DCOMWA6I-E-IMPR-TERM-ADSAO    = TERMO DE ADESAO                *
      * DCOMWA6I-E-IMPR-DECLR         = DECLARACAO                     *
      * DCOMWA6I-E-IMPR-AUTRZ         = AUTORIZACAO                    *
      * DCOMWA6I-E-IMPR-NOTA-PROMS    = NOTA PROMISSORIA               *
      * DCOMWA6I-E-IMPR-FICHA-CTBIL   = FICHA CONTABIL                 *
      *******GERAR RELATORIO NO CMFE************************************
      * DCOMWA6I-E-CDPRODT-BDSC       = CODIGO PRODUTO BRADESCO        *
      * DCOMWA6I-E-CFAML-CONTR        = CODIGO FAMILIA CONTRATO        *
      * DCOMWA6I-E-CCONTR             = CODIGO CONTRATO                *
      * DCOMWA6I-E-CSEQ-ATLZ-CONTR    = CODIGO SEQUENCIA ATUALIZ. CONT.*
      * DCOMWA6I-E-VERSAO             = VERSAO                         *
      * DCOMWA6I-E-RELEASE            = RELEASE                        *
      *******GRAVAR DOCUMENTO*******************************************
      * DCOMWA6I-E-CAG-BCRIA          = CODIGO AGENCIA                 *
      * DCOMWA6I-E-CODIGO-GCC         = CODIGO GCC                     *
      * DCOMWA6I-E-CTPLAT-IMPRE-DOCTO = CODIGO FORMULARIO BRADESCO     *
      * DCOMWA6I-E-CSGL-TPLAT-IMPRE   = NOME DO FORMULARIO BRADESCO    *
      * DCOMWA6I-E-CPSSOA-JURID-CONTR = CODIGO PESSOA JURIDICA CONTRATO*
      * DCOMWA6I-E-CTPO-CONTR-NEGOC   = CODIGO CLASSIF DO CONTRATO NEG *
      * DCOMWA6I-E-NSEQ-CONTR-NEGOC   = NUMERO SEQUENCIA CONTRATO NEG  *
      * DCOMWA6I-E-DATA-VIGENCIA      = DATA DA VIGENCIA               *
      *******CONSULTAR DOCUMENTO****************************************
      * DCOMWA6I-E-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      * DCOMWA6I-E-NPAG-TPLAT-IMPRE   = NUM. DA PAGINA DO DOCUMENTO    *
      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
      * FEMPWA6E-E-NSEQ-FORM-IMPR     = NUM. SEQUENCIAL DO FORMULARIO  *
      *                                 PARA PROCESSOS QUE ENVOLVEM    *
      *                                 MULTIPLOS FORMULARIOS          *
      *                                 SE NAO APLICAVEL, ENVIAR ZEROS *
      *******DADOS DE SESSAO********************************************
      * DCOMWA6I-E-CUSUAR-SESSAO      = CODIGO DO USUARIO              *
      * DCOMWA6I-E-CDEPEN-SESSAO      = CODIGO DA DEPENDENCIA          *
      * DCOMWA6I-E-CTERMINAL          = CODIGO DO TERMINAL             *
      * DCOMWA6I-E-CTPO-CANAL         = TIPO DO CANAL                  *
      * DCOMWA6I-E-CPSSOA-JURID       = CODIGO PESSOA JURIDICA         *
      * DCOMWA6I-E-NSEQ-UND-ORGNZ     = NUM. SEQ. UNID ORGANIZACIONAL  *
      * DCOMWA6I-E-EMPR-USUAR-TRAB    = EMPRESA USUARIO                *
      * DCOMWA6I-E-DEPEND-USUAR-TRAB  = DEPENDENCIA USUARIO            *
      *******CAMPOS DE SAIDA********************************************
      * DCOMWA6I-S-QTDE-TOT-PAG       = QTD. TOTAL DE PAG. DO DOC.     *
      * DCOMWA6I-S-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      *                                 CONFORME O DICM                *
      * DCOMWA6I-S-WTPLAT-IMPRE-DOCTO = PAG. DO DOC. PDF               *
      ******************************************************************
         05 DCOMWA6I-HEADER.
            10 DCOMWA6I-COD-LAYOUT          PIC X(008) VALUE 'DCOMWA6I'.
            10 DCOMWA6I-TAM-LAYOUT          PIC 9(005) VALUE 21436.
         05 DCOMWA6I-REGISTRO.
            10 DCOMWA6I-BLOCO-ENTRADA.

      *************************************************************
               15 DCOMWA6I-E-CTPO-ACESSO        PIC X(01).
               15 DCOMWA6I-E-POPUP              PIC X(01).
               15 DCOMWA6I-E-FORM-IMPR.
                 20 DCOMWA6I-E-IMPR-VIA-NEGOC   PIC X(01).
                 20 DCOMWA6I-E-IMPR-VIA-N-NEGOC PIC X(01).
                 20 DCOMWA6I-E-IMPR-CONTR       PIC X(01).
                 20 DCOMWA6I-E-IMPR-APOLIC-SEGUR
                                                PIC X(01).
                 20 DCOMWA6I-E-IMPR-TERM-ADSAO  PIC X(01).
                 20 DCOMWA6I-E-IMPR-DECLR       PIC X(01).
                 20 DCOMWA6I-E-IMPR-AUTRZ       PIC X(01).
                 20 DCOMWA6I-E-IMPR-NOTA-PROMS  PIC X(01).
                 20 DCOMWA6I-E-IMPR-FICHA-CTBIL PIC X(01).

      *******GERAR RELATORIOS NO DCOM **********************************
               15 DCOMWA6I-E-COD-DOCTO          PIC X(10).
               15 DCOMWA6I-E-NUM-OPER           PIC 9(13).
               15 DCOMWA6I-TIPO-VIA             PIC 9(1).
               15 DCOMWA6I-E-VERSAO             PIC 9(08).
               15 DCOMWA6I-E-RELEASE            PIC 9(01).

      *******GRAVAR DOCUMENTO NO DOCG***********************************
               15 DCOMWA6I-E-CAG-BCRIA          PIC 9(05).
               15 DCOMWA6I-E-CODIGO-GCC         PIC 9(08).
               15 DCOMWA6I-E-CTPLAT-IMPRE-DOCTO PIC 9(04).
               15 DCOMWA6I-E-CSGL-TPLAT-IMPRE   PIC X(05).
               15 DCOMWA6I-E-DATA-VIGENCIA      PIC X(10).

      *******CONSULTAR DOCUMENTO NO DOCG********************************
               15 DCOMWA6I-E-NPROT-IMPRE-DOCTO  PIC 9(10).
               15 DCOMWA6I-E-NPAG-TPLAT-IMPRE   PIC 9(05).

      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
               15 DCOMWA6I-E-NSEQ-FORM-IMPR     PIC 9(03).

      *******DADOS OBTIDOS NA SESSAO*******************************
            10 DCOMWA6I-BLOCO-SESSAO.
               15 DCOMWA6I-E-CUSUAR-SESSAO      PIC X(09).
               15 DCOMWA6I-E-CDEPEN-SESSAO      PIC 9(05).
               15 DCOMWA6I-E-CTERMINAL          PIC X(08).
               15 DCOMWA6I-E-CTPO-CANAL         PIC 9(03).
               15 DCOMWA6I-E-CPSSOA-JURID       PIC 9(10).
               15 DCOMWA6I-E-NSEQ-UND-ORGNZ     PIC 9(08).
               15 DCOMWA6I-E-EMPR-USUAR-TRAB    PIC 9(10).
               15 DCOMWA6I-E-DEPEND-USUAR-TRAB  PIC 9(08).

      *******BLOCO DE SAIDA *******O*******************************
            10 DCOMWA6I-BLOCO-SAIDA.
               15 DCOMWA6I-S-QTDE-TOT-PAG       PIC 9(05).
               15 DCOMWA6I-S-NPROT-IMPRE-DOCTO  PIC 9(10).
               15 DCOMWA6I-S-WTPLAT-IMPRE-DOCTO PIC X(20000).
