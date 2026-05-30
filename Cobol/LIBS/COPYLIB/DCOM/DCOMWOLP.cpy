      ******************************************************************
      * COPYBOOK  : DCOMWOLP                                           *
      * DESCRICAO : BOOK DE PAGINACAO - LISTA DE CONTRATOS DCOM OPBK   *
      * DATA      : 12/05/2021                                         *
      * AUTOR     : LUIS EDUARDO                                       *
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
      * DCOMWOLP-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOLP-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOLP-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOLP-E-AGENCIA          = AGENCIA DO CLIENTE               *
      * DCOMWOLP-E-CONTA            = CONTA DO CLIENTE                 *
      * DCOMWOLP-E-CSEQ-TTLAR       = TITULARIDADE DO CLIENTE          *
      * DCOMWOLP-E-CCPF-CNPJ        = PRINCIPAL CPF OU CNPJ            *
      * DCOMWOLP-E-CFLIAL-CNPJ      = FILIAL CPF OU CNPJ               *
      * DCOMWOLP-E-CCTRL-CPF-CNPJ   = CONTROLE CPF OU CNPJ             *
      *********************** DADOS DE PAGINACAO ***********************
      * DCOMWOLP-INDICADOR-PAGINACAO = INDICADOR DE PAGINACAO          *
      * OBS.: ESTE PROCESSO SO ACEITA PAGINACAO INICIAL E SEGUINTE     *
      **--------------------------------------------------------------**
      * DCOMWOLP-CHAVE-INI - CHAVE DE PESQUISA INICIAL                 *
      * DCOMWOLP-I-DANO-OPER-DESC   = ANO OPERACAO INICIAL             *
      * DCOMWOLP-I-NSEQ-OPER-DESC   = SEQUENCIA OPERACAO INICIAL       *
      * DCOMWOLP-CHAVE-FIM - CHAVE DE PESQUISA FINAL                   *
      * DCOMWOLP-F-DANO-OPER-DESC   = ANO OPERACAO FINAL               *
      * DCOMWOLP-F-NSEQ-OPER-DESC   = SEQUENCIA OPERACAO FINAL         *
      *********************** DADOS DE SAIDA ***************************
      * DCOMWOLP-COD-RETORNO = CODIGO DE RETORNO PARA PAGINACAO        *
      ******************************************************************
           05 DCOMWOLP-HEADER.
              10 DCOMWOLP-COD-LAYOUT        PIC  X(08) VALUE 'DCOMWOLP'.
              10 DCOMWOLP-TAM-LAYOUT        PIC  9(05) VALUE 200.
           05 DCOMWOLP-BLOCO-ENTRADA.
             10 DCOMWOLP-BLOCO-LISTACONTR.
                15 DCOMWOLP-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOLP-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOLP-E-DATA-FIM           PIC  X(010).
                15 DCOMWOLP-E-AGENCIA            PIC  9(004).
                15 DCOMWOLP-E-CONTA              PIC  9(007).
                15 DCOMWOLP-E-CSEQ-TTLAR         PIC  9(002).
                15 DCOMWOLP-E-CCPF-CNPJ          PIC  9(009).
                15 DCOMWOLP-E-CFLIAL-CNPJ        PIC  9(004).
                15 DCOMWOLP-E-CCTRL-CPF-CNPJ     PIC  9(002).
             10 FILLER                     PIC  X(100).
             10 DCOMWOLP-BLOCO-PAGINACAO.
                 15 DCOMWOLP-INDICADOR-PAGINACAO   PIC  X(01).
                   88 DCOMWOLP-P-INICIAL              VALUE 'I'.
                   88 DCOMWOLP-P-PRIMEIRA             VALUE 'P'.
                   88 DCOMWOLP-P-SEGUINTE             VALUE 'S'.
                   88 DCOMWOLP-P-ANTERIOR             VALUE 'A'.
                   88 DCOMWOLP-P-ULTIMA               VALUE 'U'.
               15  DCOMWOLP-CHAVE-INI.
                   20  DCOMWOLP-I-DANO-OPER-DESC   PIC  9(04).
                   20  DCOMWOLP-I-NSEQ-OPER-DESC   PIC  9(09).
               15  DCOMWOLP-CHAVE-FIM.
                   20  DCOMWOLP-F-DANO-OPER-DESC   PIC  9(04).
                   20  DCOMWOLP-F-NSEQ-OPER-DESC   PIC  9(09).
             10 DCOMWOLP-BLOCO-RETORNO.
                15 DCOMWOLP-COD-RETORNO            PIC  9(02).
