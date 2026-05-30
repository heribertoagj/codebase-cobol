      ******************************************************************
      * COPYBOOK  : DCOMWOVI                                           *
      * DESCRICAO : BOOK DE INTERFACE - LISTA PAGAMENTOS DE PARCELAS   *
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
      * DCOMWOVI-BLOCO-ENTRADA                                         *
      * DCOMWOVI-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOVI-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOVI-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOVI-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOVI-E-OPERACAO-DESC    = OPERACAO DESCONTO                *
      *----------------------------------------------------------------*
      * DCOMWOVI-S-BLOCO-SAIDA                                         *
      * DCOMWOVI-S-PARC-PAGAS       = TOTAL PARCELAS PAGAS             *
      * DCOMWOVI-S-SALDO-PEND-CONTR = SALDO TOTAL A PAGAR              *
      * DCOMWOVI-S-QTD-PAG          = QTD OCORRENCIAS - PARCELAS PAGAS *
      *# LISTA PARCELAS PAGAS                                          *
      * DCOMWOVI-S-ID-PAG           = IDENTIFICA TIPO PAGAMENTO        *
      * DCOMWOVI-S-IDENT-PAG        = PAGAMENTO PACTUADO / AVULSO      *
      * DCOMWOVI-S-ID-PARC          = NUMERO DA PARCELA PAGA           *
      * DCOMWOVI-S-DT-PAG           = DATA PAGAMENTO                   *
      * DCOMWOVI-S-MOEDA-PAG        = TIPO MOEDA                       *
      * DCOMWOVI-S-VLR-PAGO         = VALOR PAGAMENTO                  *
      * DCOMWOVI-S-QTD-TARIF        = QUANTIDADE DE TARIFAS PAGAS      *
      *# LISTA TARIFAS                                                 *
      * DCOMWOVI-S-NOME-TARIF       = NOME TARIFA                      *
      * DCOMWOVI-S-COD-TARIF        = CODIGO TARIFA                    *
      * DCOMWOVI-S-VLR-TARIF        = VALOR TARIFA                     *
      *# FIM LISTA TARIFAS                                             *
      * DCOMWOVI-S-QTD-ENCAR        = QUANTIDADE ENCARGOS PAGOS        *
      *# LISTA ENCARGOS                                                *
      * DCOMWOVI-S-TIPO-ENCAR       = TIPO ENCARGO                     *
      * DCOMWOVI-S-INF-ADC-ENCAR    = INFORMACAO ADICIONAIS            *
      * DCOMWOVI-S-VLR-ENCAR        = VALOR DO ENCARGO                 *
      *# FIM LISTA ENCARGOS                                            *
      *# FIM LISTA PARCELAS PAGAS                                      *
      ******************************************************************
          05 DCOMWOVI-HEADER.
             10 DCOMWOVI-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOVI'.
             10 DCOMWOVI-TAM-LAYOUT        PIC 9(005) VALUE 15197.
             10 DCOMWOVI-VERSAO            PIC X(005).
          05 DCOMWOVI-BLOCO-ENTRADA.
             10 DCOMWOVI-BLOCO-LISTACONTR.
                15 DCOMWOVI-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOVI-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOVI-E-DATA-FIM           PIC  X(010).
                15 DCOMWOVI-E-OPERACAO-DESC      PIC  9(013).
             10 FILLER                           PIC  X(100).
          05 DCOMWOVI-BLOCO-PAGINACAO.
             10 DCOMWOVI-INDICADOR-PAGINACAO     PIC  X(001).
                88 DCOMWOVI-P-INICIAL            VALUE 'I'.
                88 DCOMWOVI-P-PRIMEIRA           VALUE 'P'.
                88 DCOMWOVI-P-SEGUINTE           VALUE 'S'.
                88 DCOMWOVI-P-ANTERIOR           VALUE 'A'.
                88 DCOMWOVI-P-ULTIMA             VALUE 'U'.
             10 DCOMWOVI-CHAVE-INI.
                15 DCOMWOVI-I-DANO-OPER-DESC    PIC  9(004).
                15 DCOMWOVI-I-NSEQ-OPER-DESC    PIC  9(009).
                15 DCOMWOVI-I-NPCELA-DESC-COML  PIC  9(005).
                15 DCOMWOVI-I-HBAIXA-PCELA-DESC PIC  X(026).
             10 DCOMWOVI-CHAVE-FIM.
                15 DCOMWOVI-F-DANO-OPER-DESC    PIC  9(004).
                15 DCOMWOVI-F-NSEQ-OPER-DESC    PIC  9(009).
                15 DCOMWOVI-F-NPCELA-DESC-COML  PIC  9(005).
                15 DCOMWOVI-F-HBAIXA-PCELA-DESC PIC  X(026).
          05 DCOMWOVI-BLOCO-SAIDA.
             10 DCOMWOVI-S-PARC-PAGAS           PIC  9(005).
             10 DCOMWOVI-S-SALDO-PEND-CONTR     PIC  9(15)V9(4).
             10 DCOMWOVI-S-QTD-PAG              PIC  9(003).
             10 DCOMWOVI-S-OCORRENCIAS-PAG   OCCURS  020 TIMES.
                15 DCOMWOVI-S-ID-PAG            PIC  X(030).
                15 DCOMWOVI-S-IDENT-PAG         PIC  X(005).
                15 DCOMWOVI-S-ID-PARC           PIC  X(010).
                15 DCOMWOVI-S-DT-PAG            PIC  X(010).
                15 DCOMWOVI-S-MOEDA-PAG         PIC  X(003) VALUE 'BRL'.
                15 DCOMWOVI-S-VLR-PAGO          PIC  9(015)V9(4).
                15 DCOMWOVI-S-QTD-TARIF         PIC  9(003).
                15 DCOMWOVI-S-OCORRENCIAS-TARIF.
                   20 DCOMWOVI-S-NOME-TARIF     PIC  X(010).
                   20 DCOMWOVI-S-COD-TARIF      PIC  X(010).
                   20 DCOMWOVI-S-VLR-TARIF      PIC  9(015)V9(4).
                15 DCOMWOVI-S-QTD-ENCAR         PIC  9(003).
                15 DCOMWOVI-S-OCORRENCIAS-ENCAR OCCURS  006 TIMES.
                   20 DCOMWOVI-S-TIPO-ENCAR     PIC  X(035).
                   20 DCOMWOVI-S-INF-ADC-ENCAR  PIC  X(050).
                   20 DCOMWOVI-S-VLR-ENCAR      PIC  9(015)V9(4).
