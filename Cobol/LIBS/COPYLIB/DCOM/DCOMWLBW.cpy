      *****************************************************************
      * NOME BOOK : DCOMWLBW                                          *
      * DESCRICAO : BLOCO DE CONTROLE PARA CHAMADAS A SERVICOS        *
      *             FUNCIONAIS                                        *
      * DATA      : 02/2025                                           *
      * AUTOR     : THIAGO AUGUSTO                                    *
      * EMPRESA   : BRADESCO                                          *
      * COMPONENTE: SISTEMA DCOM - BRADESCO INTELIGENCIA DE NEGOCIO   *
      * TAMANHO   : 00250 BYTES                                       *
      ************************ DADOS RETORNO **************************
      * DCOMWLBW-R-COD-RETORNO       = CODIGO DE RETORNO              *
      * DCOMWLBW-R-COD-ERRO          = CODIGO DE ERRO                 *
      * DCOMWLBW-R-COD-MENSAGEM      = CODIGO DE MENSAGEM             *
      * DCOMWLBW-R-PARAMETRO         = PARAMETRO DE MENSAGEM.         *
      * FILLER                       = ESPACO PARA EXPANCAO           *
      *                                                               *
      * TAMANHO RETORNO              = 237 BYTES                      *
      *****************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO           *
      *****************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   *
      *****************************************************************
        05 DCOMWLBW-HEADER.
          10 DCOMWLBW-COD-LAYOUT         PIC X(008)   VALUE 'DCOMWLBW'.
          10 DCOMWLBW-TAM-LAYOUT         PIC 9(005)   VALUE      00250.
        05 DCOMWLBW-REGISTRO.
         07  DCOMWLBW-BLOCO-RETORNO.
          10 DCOMWLBW-R-COD-RETORNO                        PIC  9(002).
          10 DCOMWLBW-R-COD-ERRO                           PIC  X(004).
          10 DCOMWLBW-R-COD-MENSAGEM                       PIC  X(008).
         07  DCOMWLBW-BLOCO-PARAMETROS.
          10 DCOMWLBW-R-PARAMETRO                          PIC  X(150).
         07  DCOMWLBW-BLOCO-EXPANCAO.
          10 FILLER                                        PIC  X(073).
