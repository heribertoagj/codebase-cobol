      ******************************************************************
      * COPYBOOK  : DCOMWOPI                                           *
      * DESCRICAO : BOOK DE INTERFACE - LISTA DE PARCELAS OPBK         *
      * DATA      : MAIO/2021                                          *
      * AUTOR     : GABRIEL SALIM                                      *
      * EMPRESA   : BRQ                                                *
      * GRUPO     : OPEN BANKING - VS FORNECIMENTO                     *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                          *
      *----------------------------------------------------------------*
      * ALTERACOES:                                                    *
      *                                                                *
      *    DATA    NOME       DESCRICAO                                *
      * XX/XX/XXXX XXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                                                *
      *----------------------------------------------------------------*
      * DCOMWOPI-BLOCO-ENTRADA                                         *
      * DCOMWOPI-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOPI-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOPI-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOPI-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOPI-E-OPERACAO-DESC    = OPERACAO DESCONTO                *
      *----------------------------------------------------------------*
      * DCOMWOPI-BLOCO-SAIDA                                           *
      * DCOMWOPI-S-TIPO-PRAZO-TOT   = TIPO PRAZO TOTAL                 *
      * DCOMWOPI-S-PRAZO-TOT        = PRAZO TOTAL EM MESES             *
      * DCOMWOPI-S-TIPO-PRAZO-REST  = TIPO PRAZO RESTANTE              *
      * DCOMWOPI-S-PRAZO-REST       = PRAZO RESTANTE EM MESES          *
      * DCOMWOPI-S-QTD-PARC-PAG     = QUANTIDADE PARCELAS PAGAS        *
      * DCOMWOPI-S-QTD-PARC-AVCR    = QUANTIDADE PARCELAS A VENCER     *
      * DCOMWOPI-S-QTD-PARC-VENC    = QUANTIDADE PARCELAS VENCIDOS     *
      * DCOMWOPI-S-QTDE-PARCELA     = QUANTIDADE PARCELAS TOTAL        *
      * DCOMWOPI-S-QTDE-OCORR       = QUANTIDADE OCORRENCIAS           *
      * LISTA DE PARCELAS:                                             *
      * DCOMWOPI-S-DATA-VENC        = DATA VENCIMENTO DA PARCELA       *
      * DCOMWOPI-S-MOEDA            = TIPO MOEDA                       *
      * DCOMWOPI-S-VALOR-PARC       = VALOR DA PARCELA                 *
      * DCOMWOPI-S-NUM-PARC         = NUMERO DA PARCELA                *
      ******************************************************************
          05 DCOMWOPI-HEADER.
             10 DCOMWOPI-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOPI'.
             10 DCOMWOPI-TAM-LAYOUT        PIC 9(005) VALUE 03739.
             10 DCOMWOPI-VERSAO            PIC X(005).
          05 DCOMWOPI-BLOCO-ENTRADA.
             10 DCOMWOPI-BLOCO-LISTACONTR.
                15 DCOMWOPI-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOPI-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOPI-E-DATA-FIM           PIC  X(010).
                15 DCOMWOPI-E-OPERACAO-DESC      PIC  9(013).
             10 FILLER                           PIC  X(100).
          05 DCOMWOPI-BLOCO-PAGINACAO.
             10 DCOMWOPI-INDICADOR-PAGINACAO     PIC  X(001).
                88 DCOMWOPI-P-INICIAL            VALUE 'I'.
                88 DCOMWOPI-P-PRIMEIRA           VALUE 'P'.
                88 DCOMWOPI-P-SEGUINTE           VALUE 'S'.
                88 DCOMWOPI-P-ANTERIOR           VALUE 'A'.
                88 DCOMWOPI-P-ULTIMA             VALUE 'U'.
             10 DCOMWOPI-CHAVE-INI.
                15  DCOMWOPI-I-DANO-OPER-DESC    PIC  9(004).
                15  DCOMWOPI-I-NSEQ-OPER-DESC    PIC  9(009).
                15  DCOMWOPI-I-NPCELA-DESC-COML  PIC  9(005).
             10 DCOMWOPI-CHAVE-FIM.
                15  DCOMWOPI-F-DANO-OPER-DESC    PIC  9(004).
                15  DCOMWOPI-F-NSEQ-OPER-DESC    PIC  9(009).
                15  DCOMWOPI-F-NPCELA-DESC-COML  PIC  9(005).
          05 DCOMWOPI-BLOCO-SAIDA.
             10 DCOMWOPI-S-TIPO-PRAZO-TOT  PIC  X(003) VALUE 'MES'.
             10 DCOMWOPI-S-PRAZO-TOT       PIC  9(006).
             10 DCOMWOPI-S-TIPO-PRAZO-REST PIC  X(003) VALUE 'MES'.
             10 DCOMWOPI-S-PRAZO-REST      PIC  9(006).
             10 DCOMWOPI-S-QTD-PARC-PAG    PIC  9(005).
             10 DCOMWOPI-S-QTD-PARC-AVCR   PIC  9(005).
             10 DCOMWOPI-S-QTD-PARC-VENC   PIC  9(005).
             10 DCOMWOPI-S-QTDE-PARCELA    PIC  9(005).
             10 DCOMWOPI-S-QTDE-OCORR      PIC  9(003).
             10 DCOMWOPI-S-OCORRENCIAS  OCCURS  100 TIMES.
                15 DCOMWOPI-S-DATA-VENC         PIC  X(010).
                15 DCOMWOPI-S-MOEDA             PIC  X(003) VALUE 'BRL'.
                15 DCOMWOPI-S-VALOR-PARC        PIC  9(15)V9(2).
                15 DCOMWOPI-S-NUM-PARC          PIC  9(005).
