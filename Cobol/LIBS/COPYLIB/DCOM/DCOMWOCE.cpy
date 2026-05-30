      ******************************************************************
      * COPYBOOK  : DCOMWOCE                                           *
      * DESCRICAO : BOOK DE INTERFACE - DETALHE  CONTRATOS DCOM OPBK   *
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
      * DCOMWOCE-VERSAO             = VERSO DO MODULOS(N OBRIGATORIO)  *
      * DCOMWOCE-E-DSOLTC-CONS-DADOS= DATA DA CONSULTA                 *
      * DCOMWOCE-E-DATA-INICIO      = DATA INICIAL PESQUISA            *
      * DCOMWOCE-E-DATA-FINAL       = DATA FINAL PESQUISA              *
      * DCOMWOCE-E-OPERACAO         = OPERACAO DESCONTO                *
      *----------------------------------------------------------------*
          05 DCOMWOCE-HEADER.
             10 DCOMWOCE-COD-LAYOUT        PIC X(008) VALUE 'DCOMWOCE'.
             10 DCOMWOCE-TAM-LAYOUT        PIC 9(005) VALUE 00161.
             10 DCOMWOCE-VERSAO            PIC X(005).
          05 DCOMWOCE-BLOCO-ENTRADA.
             10 DCOMWOCE-BLOCO-DETCONTR.
                15 DCOMWOCE-E-DSOLTC-CONS-DADOS  PIC  X(010).
                15 DCOMWOCE-E-DATA-INICIO        PIC  X(010).
                15 DCOMWOCE-E-DATA-FIM           PIC  X(010).
                15 DCOMWOCE-E-OPERACAO           PIC  9(013).
             10 FILLER                     PIC  X(100).
