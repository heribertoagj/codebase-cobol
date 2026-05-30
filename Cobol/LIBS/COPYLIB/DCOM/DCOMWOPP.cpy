      ******************************************************************
      * COPYBOOK  : DCOMWOPP                                           *
      * DESCRICAO : BOOK DE PAGINACAO - LISTA DE PARCELAS DCOM OPBK    *
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
      * DCOMWOPP-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOPP-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOPP-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOPP-E-OPERACAO-DESC    = OPERACAO DESCONTO                *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMWOPP-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: ESTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      **--------------------------------------------------------------**
      * DCOMWOPP-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMWOPP-I-DANO-OPER-DESC   = ANO OPERACAO INICIAL             *
      * DCOMWOPP-I-NSEQ-OPER-DESC   = SEQUENCIA OPERACAO INICIAL       *
      * DCOMWOPP-I-NPCELA-DESC-COML = NUMERO DA PARCELA  INICIAL       *
      * DCOMWOPP-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMWOPP-F-DANO-OPER-DESC   = ANO OPERACAO FINAL               *
      * DCOMWOPP-F-NSEQ-OPER-DESC   = SEQUENCIA OPERACAO FINAL         *
      * DCOMWOPP-F-NPCELA-DESC-COML = NUMERO DA PARCELA  FINAL         *
      *********************** DADOS DE SAIDA ***************************
      * DCOMWOPP-COD-RETORNO = CODIGO DE RETORNO PARA PAGINACAO        *
      ******************************************************************
           05 DCOMWOPP-HEADER.
              10 DCOMWOPP-COD-LAYOUT        PIC  X(08) VALUE 'DCOMWOPP'.
              10 DCOMWOPP-TAM-LAYOUT        PIC  9(05) VALUE 195.
           05 DCOMWOPP-BLOCO-ENTRADA.
             10 DCOMWOPP-BLOCO-LISTACONTR.
                15 DCOMWOPP-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOPP-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOPP-E-DATA-FIM           PIC  X(010).
                15 DCOMWOPP-E-OPERACAO-DESC      PIC  9(013).
             10 FILLER                     PIC  X(100).
             10 DCOMWOPP-BLOCO-PAGINACAO.
                 15 DCOMWOPP-INDICADOR-PAGINACAO   PIC  X(01).
                   88 DCOMWOPP-P-INICIAL              VALUE 'I'.
                   88 DCOMWOPP-P-PRIMEIRA             VALUE 'P'.
                   88 DCOMWOPP-P-SEGUINTE             VALUE 'S'.
                   88 DCOMWOPP-P-ANTERIOR             VALUE 'A'.
                   88 DCOMWOPP-P-ULTIMA               VALUE 'U'.
               15  DCOMWOPP-CHAVE-INI.
                   20  DCOMWOPP-I-DANO-OPER-DESC   PIC  9(04).
                   20  DCOMWOPP-I-NSEQ-OPER-DESC   PIC  9(09).
                   20  DCOMWOPP-I-NPCELA-DESC-COML PIC  9(05).
               15  DCOMWOPP-CHAVE-FIM.
                   20  DCOMWOPP-F-DANO-OPER-DESC   PIC  9(04).
                   20  DCOMWOPP-F-NSEQ-OPER-DESC   PIC  9(09).
                   20  DCOMWOPP-F-NPCELA-DESC-COML PIC  9(05).
             10 DCOMWOPP-BLOCO-RETORNO.
                15 DCOMWOPP-COD-RETORNO            PIC  9(02).
