      ******************************************************************
      * COPYBOOK  : DCOMWOLI                                           *
      * DESCRICAO : BOOK DE INTERFACE - LISTA DE CONTRATOS CRD. OPBK   *
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
      *----------------------------------------------------------------*
      * DCOMWOLI-BLOCO-ENTRADA                                         *
      * DCOMWOLI-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOLI-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOLI-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOLI-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOLI-E-AGENCIA          = AGENCIA DO CLIENTE               *
      * DCOMWOLI-E-CONTA            = CONTA DO CLIENTE                 *
      * DCOMWOLI-E-CSEQ-TTLAR       = TITULARIDADE DO CLIENTE          *
      * DCOMWOLI-E-CCPF-CNPJ        = PRINCIPAL CPF OU CNPJ            *
      * DCOMWOLI-E-CFLIAL-CNPJ      = FILIAL CPF OU CNPJ               *
      * DCOMWOLI-E-CCTRL-CPF-CNPJ   = CONTROLE CPF OU CNPJ             *
      *----------------------------------------------------------------*
      * DCOMWOLI-BLOCO-SAIDA                                           *
      * DCOMWOLI-S-CMODLD-RZ-CTBIL  = TIPO E MODALIDADE RISC           *
      *                                2 PRIMEIRAS TIPO MODALIDADE     *
      *                                2 ULTIMAS MODALIDADE            *
      *  DCOMWOLI-S-DEXPARA-MOD     = DE X PARA DAS MODALIDADES DO RISC*
      *                               PARA O APRESENTADO AO CLIENTE    *
      *  DCOMWOLI-S-WID-PDRAO-OPER  = NUMERO DO CONTRATO IPOC          *
      *  DCOMWOLI-S-SIST-ORIGEM     = SIGLA SISTEMA ORIGEM             *
      *  DCOMWOLI-S-OPERACAO        = ANO/SEQ OPERACAO DESCONTO        *
      *  DCOMWOLS-S-DT-QUITACAO     = DATA QUITACAO DO CONTRATO        *
      *                               'BRANCOS' - CONTRATO ATIVO       *
      *  DCOMWOLI-S-CARTEIRA        = CARTEIRA RISC                    *
      ******************************************************************
          05 DCOMWOLI-HEADER.
             10 DCOMWOLI-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOLI'.
             10 DCOMWOLI-TAM-LAYOUT        PIC 9(005) VALUE 13906.
             10 DCOMWOLI-VERSAO            PIC X(005).
          05 DCOMWOLI-BLOCO-ENTRADA.
             10 DCOMWOLI-BLOCO-LISTACONTR.
                15 DCOMWOLI-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOLI-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOLI-E-DATA-FIM           PIC  X(010).
                15 DCOMWOLI-E-AGENCIA            PIC  9(004).
                15 DCOMWOLI-E-CONTA              PIC  9(007).
                15 DCOMWOLI-E-CSEQ-TTLAR         PIC  9(002).
                15 DCOMWOLI-E-CCPF-CNPJ          PIC  9(009).
                15 DCOMWOLI-E-CFLIAL-CNPJ        PIC  9(004).
                15 DCOMWOLI-E-CCTRL-CPF-CNPJ     PIC  9(002).
             10 FILLER                           PIC  X(100).
          05 DCOMWOLI-BLOCO-PAGINACAO.
             10 DCOMWOLI-INDICADOR-PAGINACAO     PIC  X(001).
                88 DCOMWOLI-P-INICIAL            VALUE 'I'.
                88 DCOMWOLI-P-PRIMEIRA           VALUE 'P'.
                88 DCOMWOLI-P-SEGUINTE           VALUE 'S'.
                88 DCOMWOLI-P-ANTERIOR           VALUE 'A'.
                88 DCOMWOLI-P-ULTIMA             VALUE 'U'.
             10 DCOMWOLI-CHAVE-INI.
                15  DCOMWOLI-I-DANO-OPER-DESC    PIC  9(004).
                15  DCOMWOLI-I-NSEQ-OPER-DESC    PIC  9(009).
             10 DCOMWOLI-CHAVE-FIM.
                15  DCOMWOLI-F-DANO-OPER-DESC    PIC  9(004).
                15  DCOMWOLI-F-NSEQ-OPER-DESC    PIC  9(009).
          05 DCOMWOLI-BLOCO-SAIDA.
             10 DCOMWOLI-S-QTDE-CONTRATO         PIC  9(003).
             10 DCOMWOLI-S-OCORRENCIAS     OCCURS 100 TIMES.
                15 DCOMWOLI-S-CMODLD-RZ-CTBIL    PIC  9(004).
                15 DCOMWOLI-S-DEXPARA-MOD        PIC  X(036).
                15 DCOMWOLI-S-WID-PDRAO-OPER     PIC  X(067).
                15 DCOMWOLI-S-SIST-ORIGEM        PIC  X(004).
                15 DCOMWOLI-S-OPERACAO           PIC  9(013).
                15 DCOMWOLI-S-DT-QUITACAO        PIC  X(010).
                15 DCOMWOLI-S-CARTEIRA           PIC  X(003).
