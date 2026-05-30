      *****************************************************************
      * NOME BOOK : DCOMW002                                          *
      * DESCRICAO : BLOCO DE CONTROLE PARA CHAMADAS A SERVICOS        *
      *             FUNCIONAIS - NOVO PADRAO                          *
      * DATA      : 05/2021                                           *
      * AUTOR     : LUIS EDUARDO                                      *
      * EMPRESA   : BRQ                                               *
      * COMPONENTE: DCOM - DESCONTO COMERCIAL                         *
      * TAMANHO   : 00250 BYTES                                       *
      *                                                               *
      ************************ DADOS RETORNO **************************
      * DCOMW002-COD-RETORNO         = CODIGO DE RETORNO              *
      * DCOMW002-COD-ERRO            = CODIGO DE ERRO                 *
      * DCOMW002-COD-MENSAGEM        = CODIGO DE MENSAGEM             *
      * DCOMW002-PARAMETRO           = PARAMETRO DE MENSAGEM.         *
      * FILLER                       = ESPACO PARA EXPANCAO           *
      *                                                               *
      * TAMANHO RETORNO              = 250 BYTES                      *
      *                                                               *
      *****************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO           *
      *****************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   *
      *****************************************************************
        05 DCOMW002-HEADER.
          10 DCOMW002-COD-LAYOUT         PIC X(008)   VALUE 'DCOMW002'.
          10 DCOMW002-TAM-LAYOUT         PIC 9(005)   VALUE      00250.
        05 DCOMW002-REGISTRO.
         07  DCOMW002-BLOCO-RETORNO.
          10 DCOMW002-COD-RETORNO                          PIC  9(002).
          10 DCOMW002-COD-ERRO                             PIC  X(004).
          10 DCOMW002-COD-MENSAGEM                         PIC  X(008).
         07  DCOMW002-BLOCO-PARAMETROS.
          10 DCOMW002-PARAMETRO                            PIC  X(220).
          10 DCOMW002-MSG-VARIAVEL                         PIC  X(001).
         07  DCOMW002-BLOCO-EXPANCAO.
          10 FILLER                                        PIC  X(002).
