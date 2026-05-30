      ******************************************************************
      * COPYBOOK  : DCOMWOLE                                           *
      * DESCRICAO : BOOK DE INTERFACE - LISTA DE CONTRATOS DCOM OPBK   *
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
      * DCOMWOLE-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOLE-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOLE-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOLE-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOLE-E-AGENCIA          = AGENCIA DO CLIENTE               *
      * DCOMWOLE-E-CONTA            = CONTA DO CLIENTE                 *
      * DCOMWOLE-E-CSEQ-TTLAR       = TITULARIDADE DO CLIENTE          *
      * DCOMWOLE-E-CCPF-CNPJ        = PRINCIPAL CPF OU CNPJ            *
      * DCOMWOLE-E-CFLIAL-CNPJ      = FILIAL CPF OU CNPJ               *
      * DCOMWOLE-E-CCTRL-CPF-CNPJ   = CONTROLE CPF OU CNPJ             *
      *----------------------------------------------------------------*
          05 DCOMWOLE-HEADER.
             10 DCOMWOLE-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOLE'.
             10 DCOMWOLE-TAM-LAYOUT        PIC 9(005) VALUE 00176.
             10 DCOMWOLE-VERSAO            PIC X(005).
          05 DCOMWOLE-BLOCO-ENTRADA.
             10 DCOMWOLE-BLOCO-LISTACONTR.
                15 DCOMWOLE-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOLE-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOLE-E-DATA-FIM           PIC  X(010).
                15 DCOMWOLE-E-AGENCIA            PIC  9(004).
                15 DCOMWOLE-E-CONTA              PIC  9(007).
                15 DCOMWOLE-E-CSEQ-TTLAR         PIC  9(002).
                15 DCOMWOLE-E-CCPF-CNPJ          PIC  9(009).
                15 DCOMWOLE-E-CFLIAL-CNPJ        PIC  9(004).
                15 DCOMWOLE-E-CCTRL-CPF-CNPJ     PIC  9(002).
             10 FILLER                     PIC  X(100).
