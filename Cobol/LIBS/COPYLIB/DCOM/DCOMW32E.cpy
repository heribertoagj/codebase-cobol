      ******************************************************************
      * NOME BOOK : DCOMW32E                                           *
      * DESCRICAO : BOOK DE ENTRADA FEMP1A6C                           *
      * FUNCAO    : SALVAR CAMPOS RELACIONADOS FORMULARIO E DICM       *
      *             RETORNAR PDF EM BINARIO COM AS ALTERACOES          *
      * TIPO      : COORDENADOR                                        *
      * DATA      : 04/04/2023                                         *
      * AUTOR     : RODRIGO PEREIRA                                    *
      * EMPRESA   : FOURSYS                                            *
      * SISTEMA   : DCOM                                               *
      * TAMANHO   : 00112 BYTES                                        *
      ******************************************************************
      * DCOMW32E-E-CTPO-ACESSO        = DEFINE O COMPORTAMENTO DO PROG.*
      *                                 G - GRAVAR DOCUMENTO           *
      *                                 C - CONSULTAR DOCUMENTO        *
      * DCOMW32E-E-POPUP              = INDICA SE A IMPRESSAO DEVE SER *
      *                                 COM ASS. DO CREDOR             *
      *                                                                *
      * OS CAMPOS ABAIXO DEVEM SER PREENCHIDOS COM S/N, PARA SEREM     *
      * IMPRESSOS                                                      *
      * DCOMW32E-E-IMPR-VIA-NEGOC     = VIA NEGOCIACAO                 *
      * DCOMW32E-E-IMPR-VIA-N-NEGOC   = VIA NAO NEGOCIAVEL             *
      * DCOMW32E-E-IMPR-CONTR         = CONTRATO                       *
      * DCOMW32E-E-IMPR-APOLIC-SEGUR  = APOLICE DE SEGURO              *
      * DCOMW32E-E-IMPR-TERM-ADSAO    = TERMO DE ADESAO                *
      * DCOMW32E-E-IMPR-DECLR         = DECLARACAO                     *
      * DCOMW32E-E-IMPR-AUTRZ         = AUTORIZACAO                    *
      * DCOMW32E-E-IMPR-NOTA-PROMS    = NOTA PROMISSORIA               *
      * DCOMW32E-E-IMPR-FICHA-CTBIL   = FICHA CONTABIL                 *
      *******GERAR RELATORIO NO CMFE************************************
      * DCOMW32E-E-VERSAO             = VERSAO                         *
      * DCOMW32E-E-RELEASE            = RELEASE                        *
      *******GRAVAR DOCUMENTO*******************************************
      * DCOMW32E-E-DATA-VIGENCIA      = DATA DA VIGENCIA               *
      *******CONSULTAR DOCUMENTO****************************************
      * DCOMW32E-E-NPROT-IMPRE-DOCTO  = NUM. DO PROTOCOLO DE IMPRESSAO *
      * DCOMW32E-E-NPAG-TPLAT-IMPRE   = NUM. DA PAGINA DO DOCUMENTO    *
      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
      * DCOMW32E-E-NSEQ-FORM-IMPR     = NUM. SEQUENCIAL DO FORMULARIO  *
      *                                 PARA PROCESSOS QUE ENVOLVEM    *
      *                                 MULTIPLOS FORMULARIOS          *
      *                                 SE NAO APLICAVEL, ENVIAR ZEROS *
      ******************************************************************
         05 DCOMW32E-HEADER.
            10 DCOMW32E-COD-LAYOUT          PIC X(008) VALUE 'DCOMW32E'.
            10 DCOMW32E-TAM-LAYOUT          PIC 9(005) VALUE 00112.
         05 DCOMW32E-REGISTRO.
            10 DCOMW32E-BLOCO-ENTRADA.
             15 DCOMW32E-E-CTPO-ACESSO        PIC X(01).
             15 DCOMW32E-E-POPUP              PIC X(01).
             15 DCOMW32E-E-FORM-IMPR.
               20 DCOMW32E-E-IMPR-VIA-NEGOC   PIC X(01).
               20 DCOMW32E-E-IMPR-VIA-N-NEGOC PIC X(01).
               20 DCOMW32E-E-IMPR-CONTR       PIC X(01).
               20 DCOMW32E-E-IMPR-APOLIC-SEGUR
                                              PIC X(01).
               20 DCOMW32E-E-IMPR-TERM-ADSAO  PIC X(01).
               20 DCOMW32E-E-IMPR-DECLR       PIC X(01).
               20 DCOMW32E-E-IMPR-AUTRZ       PIC X(01).
               20 DCOMW32E-E-IMPR-NOTA-PROMS  PIC X(01).
               20 DCOMW32E-E-IMPR-FICHA-CTBIL PIC X(01).

      *******GERAR RELATORIOS NO DCOM **********************************
             15 DCOMW32E-E-COD-DOCTO          PIC X(10).
             15 DCOMW32E-E-NUM-OPER           PIC 9(13).
             15 DCOMW32E-TIPO-VIA             PIC 9(1).
             15 DCOMW32E-E-VERSAO             PIC 9(08).
             15 DCOMW32E-E-RELEASE            PIC 9(01).

      *******GRAVAR DOCUMENTO NO DOCG***********************************
             15 DCOMW32E-E-CAG-BCRIA          PIC 9(05).
             15 DCOMW32E-E-CODIGO-GCC         PIC 9(08).
             15 DCOMW32E-E-CTPLAT-IMPRE-DOCTO PIC 9(04).
             15 DCOMW32E-E-CSGL-TPLAT-IMPRE   PIC X(05).
             15 DCOMW32E-E-DATA-VIGENCIA      PIC X(10).

      *******CONSULTAR DOCUMENTO NO DOCG********************************
             15 DCOMW32E-E-NPROT-IMPRE-DOCTO  PIC 9(10).
             15 DCOMW32E-E-NPAG-TPLAT-IMPRE   PIC 9(05).

      *******IMPRESSAO DE MULTIPLOS FORMULARIOS*************************
             15 DCOMW32E-E-NSEQ-FORM-IMPR     PIC 9(03).

      *******DADOS DA PARCELA*******************************************
             15 DCOMW32E-PARCELA              PIC 9(05).

