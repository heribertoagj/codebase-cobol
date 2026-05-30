      ******************************************************************
      * COPYBOOK  : DCOMWOVP                                           *
      * DESCRICAO : BOOK DE PAGINACAO - LISTA PAGAMENTOS DE PARCELAS   *
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
      *********************** DADOS DE ENTRADA *************************
      * DCOMWOVP-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOVP-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOVP-E-DATA-FIM         = DATA FINAL PESQUISA              *
      * DCOMWOVP-E-OPERACAO-DESC    = OPERACAO DESCONTO                *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMWOVP-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: ESTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      **--------------------------------------------------------------**
      * DCOMWOVP-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMWOVP-I-DANO-OPER-DESC   = ANO OPERACAO INICIAL             *
      * DCOMWOVP-I-NSEQ-OPER-DESC   = SEQUENCIA OPERACAO INICIAL       *
      * DCOMWOVP-I-NPCELA-DESC-COML = NUMERO PARCELA     INICIAL       *
      * DCOMWOVP-I-HBAIXA-PCELA-DESC= DATA HORA DA BAIXA INICIAL       *
      * DCOMWOVP-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMWOVP-F-DANO-OPER-DESC   = ANO OPERACAO FINAL               *
      * DCOMWOVP-F-NSEQ-OPER-DESC   = SEQUENCIA OPERACAO FINAL         *
      * DCOMWOVP-F-NPCELA-DESC-COML = NUMERO PARCELA     FINAL         *
      * DCOMWOVP-F-HBAIXA-PCELA-DESC= DATA HORA DA BAIXA FINAL         *
      *********************** DADOS DE SAIDA ***************************
      * DCOMWOVP-COD-RETORNO = CODIGO DE RETORNO PARA PAGINACAO        *
      ******************************************************************
           05 DCOMWOVP-HEADER.
              10 DCOMWOVP-COD-LAYOUT        PIC  X(08) VALUE 'DCOMWOVP'.
              10 DCOMWOVP-TAM-LAYOUT        PIC  9(05) VALUE 247.
           05 DCOMWOVP-BLOCO-ENTRADA.
             10 DCOMWOVP-BLOCO-LISTACONTR.
                15 DCOMWOVP-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOVP-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOVP-E-DATA-FIM           PIC  X(010).
                15 DCOMWOVP-E-OPERACAO-DESC      PIC  9(013).
             10 FILLER                           PIC  X(100).
             10 DCOMWOVP-BLOCO-PAGINACAO.
                 15 DCOMWOVP-INDICADOR-PAGINACAO   PIC  X(01).
                   88 DCOMWOVP-P-INICIAL              VALUE 'I'.
                   88 DCOMWOVP-P-PRIMEIRA             VALUE 'P'.
                   88 DCOMWOVP-P-SEGUINTE             VALUE 'S'.
                   88 DCOMWOVP-P-ANTERIOR             VALUE 'A'.
                   88 DCOMWOVP-P-ULTIMA               VALUE 'U'.
               15  DCOMWOVP-CHAVE-INI.
                   20  DCOMWOVP-I-DANO-OPER-DESC    PIC  9(04).
                   20  DCOMWOVP-I-NSEQ-OPER-DESC    PIC  9(09).
                   20  DCOMWOVP-I-NPCELA-DESC-COML  PIC  9(05).
                   20  DCOMWOVP-I-HBAIXA-PCELA-DESC PIC  X(26).
               15  DCOMWOVP-CHAVE-FIM.
                   20  DCOMWOVP-F-DANO-OPER-DESC    PIC  9(04).
                   20  DCOMWOVP-F-NSEQ-OPER-DESC    PIC  9(09).
                   20  DCOMWOVP-F-NPCELA-DESC-COML  PIC  9(05).
                   20  DCOMWOVP-F-HBAIXA-PCELA-DESC PIC  X(26).
             10 DCOMWOVP-BLOCO-RETORNO.
                15 DCOMWOVP-COD-RETORNO             PIC  9(02).
