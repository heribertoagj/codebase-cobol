      ******************************************************************
      * COPYBOOK  : DCOMWOPE                                           *
      * DESCRICAO : BOOK DE INTERFACE - LISTA DE PARCELAS DA OPERACAO  *
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
      * DCOMWOPE-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOPE-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOPE-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOPE-E-DATA-FIM         = DATA FINAL PESQUISA              *
      * DCOMWOPE-E-OPERACAO-DESC    = NUMERO DA OPERACAO DESCONTO      *
      *                               (ANO + SEQUENCIA)                *
      *----------------------------------------------------------------*
          05 DCOMWOPE-HEADER.
             10 DCOMWOPE-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOPE'.
             10 DCOMWOPE-TAM-LAYOUT        PIC 9(005) VALUE 00161.
             10 DCOMWOPE-VERSAO            PIC X(005).
          05 DCOMWOPE-BLOCO-ENTRADA.
             10 DCOMWOPE-BLOCO-LISTACONTR.
                15 DCOMWOPE-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOPE-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOPE-E-DATA-FIM           PIC  X(010).
                15 DCOMWOPE-E-OPERACAO-DESC      PIC  9(013).
             10 FILLER                     PIC  X(100).
