      ******************************************************************
      * NOME BOOK : DCOMWA6E                                           *
      * DESCRICAO : BOOK DE ENTRADA FEMP1A6C                           *
      * FUNCAO    : SALVAR CAMPOS RELACIONADOS FORMULARIO E DICM       *
      *             RETORNAR PDF EM BINARIO COM AS ALTERACOES          *
      * TIPO      : COORDENADOR                                        *
      * DATA      : 03/12/2022                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * EMPRESA   : FOURSYS                                            *
      * SISTEMA   : DCOM                                               *
      * TAMANHO   : 00107 BYTES                                        *
      ******************************************************************
      * DCOMWA6E-E-CTPO-ACESSO        = DEFINE O COMPORTAMENTO DO PROG.*
      *                                 G - GRAVAR DOCUMENTO           *
      *                                 C - CONSULTAR DOCUMENTO        *
      * DCOMWA6E-E-POPUP              = INDICA SE A IMPRESSAO DEVE SER *
      *                                 COM ASS. DO CREDOR             *
      *                                                                *
      * OS CAMPOS ABAIXO DEVEM SER PREENCHIDOS COM S/N, PARA SEREM     *
      * IMPRESSOS                                                      *
      * DCOMWA6E-E-IMPR-VIA-NEGOC     = VIA NEGOCIACAO                 *
      * DCOMWA6E-E-IMPR-VIA-N-NEGOC   = VIA NAO NEGOCIAVEL             *
      * DCOMWA6E-E-IMPR-CONTR         = CONTRATO                       *
      * DCOMWA6E-E-IMPR-APOLIC-SEGUR  = APOLICE DE SEGURO              *
      * DCOMWA6E-E-IMPR-TERM-ADSAO    = TERMO DE ADESAO                *
      * DCOMWA6E-E-IMPR-DECLR         = DECLARACAO                     *
      * DCOMWA6E-E-IMPR-AUTRZ         = AUTORIZACAO                    *
      * DCOMWA6E-E-IMPR-NOTA-PROMS    = NOTA PROMISSORIA               *
      * DCOMWA6E-E-IMPR-FICHA-CTBIL   = FICHA CONTABIL                 *
      *******GERAR RELATORIO NO CMFE************************************
      * DCOMWA6E-E-VERSAO             = VERSAO                         *
      * DCOMWA6E-E-RELEASE            = RELEASE                        *
      *******GRAVAR DOCUMENTO*******************************************
      * DCOMWA6E-E-DATA-VIGENCIA      = DATA DA VIGENCIA               *
      *******CONSULTAR DOCUMENTO****************************************
      * DCOMWA6E-E-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      * DCOMWA6E-E-NPAG-TPLAT-IMPRE   = NUM. DA PAGINA DO DOCUMENTO    *
      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
      * DCOMWA6E-E-NSEQ-FORM-IMPR     = NUM. SEQUENCIAL DO FORMULARIO  *
      *                                 PARA PROCESSOS QUE ENVOLVEM    *
      *                                 MULTIPLOS FORMULARIOS          *
      *                                 SE NAO APLICAVEL, ENVIAR ZEROS *
      ******************************************************************
         05 DCOMWA6E-HEADER.
            10 DCOMWA6E-COD-LAYOUT          PIC X(008) VALUE 'DCOMWA6E'.
            10 DCOMWA6E-TAM-LAYOUT          PIC 9(005) VALUE 00107.
         05 DCOMWA6E-REGISTRO.
            10 DCOMWA6E-BLOCO-ENTRADA.
             15 DCOMWA6E-E-CTPO-ACESSO        PIC X(01).
             15 DCOMWA6E-E-POPUP              PIC X(01).
             15 DCOMWA6E-E-FORM-IMPR.
               20 DCOMWA6E-E-IMPR-VIA-NEGOC   PIC X(01).
               20 DCOMWA6E-E-IMPR-VIA-N-NEGOC PIC X(01).
               20 DCOMWA6E-E-IMPR-CONTR       PIC X(01).
               20 DCOMWA6E-E-IMPR-APOLIC-SEGUR
                                              PIC X(01).
               20 DCOMWA6E-E-IMPR-TERM-ADSAO  PIC X(01).
               20 DCOMWA6E-E-IMPR-DECLR       PIC X(01).
               20 DCOMWA6E-E-IMPR-AUTRZ       PIC X(01).
               20 DCOMWA6E-E-IMPR-NOTA-PROMS  PIC X(01).
               20 DCOMWA6E-E-IMPR-FICHA-CTBIL PIC X(01).

      *******GERAR RELATORIOS NO DCOM **********************************
             15 DCOMWA6E-E-COD-DOCTO          PIC X(10).
             15 DCOMWA6E-E-NUM-OPER           PIC 9(13).
             15 DCOMWA6E-TIPO-VIA             PIC 9(1).
             15 DCOMWA6E-E-VERSAO             PIC 9(08).
             15 DCOMWA6E-E-RELEASE            PIC 9(01).

      *******GRAVAR DOCUMENTO NO DOCG***********************************
             15 DCOMWA6E-E-CAG-BCRIA          PIC 9(05).
             15 DCOMWA6E-E-CODIGO-GCC         PIC 9(08).
             15 DCOMWA6E-E-CTPLAT-IMPRE-DOCTO PIC 9(04).
             15 DCOMWA6E-E-CSGL-TPLAT-IMPRE   PIC X(05).
             15 DCOMWA6E-E-DATA-VIGENCIA      PIC X(10).

      *******CONSULTAR DOCUMENTO NO DOCG********************************
             15 DCOMWA6E-E-NPROT-IMPRE-DOCTO  PIC 9(10).
             15 DCOMWA6E-E-NPAG-TPLAT-IMPRE   PIC 9(05).

      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
             15 DCOMWA6E-E-NSEQ-FORM-IMPR     PIC 9(03).

