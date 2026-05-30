      ******************************************************************
      * NOME BOOK : CRURW00W - BOOK DE CONTROLE A ACESSO A FUNCIONAIS  *
      * DESCRICAO :                                                    *
      * DATA      : 12/08/2010                                         *
      * AUTOR     : WESLEY FREIRE SILVA                                *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE:                                                    *
      * TAMANHO   : 27 BYTES                                           *
      ************************ DADOS DE RETORNO ************************
      * CRURW00W-COD-RETORNO         = CODIGO DE RETORNO               *
      * CRURW00W-COD-ERRO            = CODIGO DE ERRO                  *
      * CRURW00W-COD-MENSAGEM        = CODIGO DE MENSAGEM              *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CRURW00W-HEADER.
             10 CRURW00W-COD-LAYOUT        PIC X(008) VALUE 'CRURW00W'.
             10 CRURW00W-TAM-LAYOUT        PIC 9(005) VALUE 27.
           05 CRURW00W-REGISTRO.
             10 CRURW00W-BLOCO-RETORNO.
               15 CRURW00W-COD-RETORNO             PIC 9(002).
               15 CRURW00W-COD-ERRO                PIC X(004).
               15 CRURW00W-COD-MENSAGEM            PIC X(008).
