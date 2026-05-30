      ******************************************************************
      * COPYBOOK  : DCOMWOVE                                           *
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
      * DCOMWOVE-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOVE-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOVE-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOVE-E-DATA-FIM         = DATA FINAL PESQUISA              *
      * DCOMWOVE-E-OPERACAO-DESC    = NUMERO DA OPERACAO DESCONTO      *
      *                               (ANO + SEQUENCIA)                *
      *----------------------------------------------------------------*
          05 DCOMWOVE-HEADER.
             10 DCOMWOVE-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOVE'.
             10 DCOMWOVE-TAM-LAYOUT        PIC 9(005) VALUE 00161.
             10 DCOMWOVE-VERSAO            PIC X(005).
          05 DCOMWOVE-BLOCO-ENTRADA.
             10 DCOMWOVE-BLOCO-LISTACONTR.
                15 DCOMWOVE-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOVE-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOVE-E-DATA-FIM           PIC  X(010).
                15 DCOMWOVE-E-OPERACAO-DESC      PIC  9(013).
             10 FILLER                     PIC  X(100).
