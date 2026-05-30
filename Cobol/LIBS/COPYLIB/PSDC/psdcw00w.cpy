      ******************************************************************
      * NOME BOOK : PSDCW00W - BOOK DE CONTROLE A ACESSO A FUNCIONAIS  *
      * DESCRICAO :                                                    *
      * DATA      : 21/02/2010                                         *
      * AUTOR     : EVALDO OLIVEIRA                                    *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE:                                                    *
      * TAMANHO   : 27 BYTES                                           *
      ************************ DADOS DE RETORNO ************************
      * PSDCW00W-COD-RETORNO         = CODIGO DE RETORNO               *
      * PSDCW00W-COD-ERRO            = CODIGO DE ERRO                  *
      * PSDCW00W-COD-MENSAGEM        = CODIGO DE MENSAGEM              *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 PSDCW00W-HEADER.
             10 PSDCW00W-COD-LAYOUT    PIC X(008)  VALUE 'PSDCW00W'.
             10 PSDCW00W-TAM-LAYOUT    PIC 9(005)  VALUE 27.
           05 PSDCW00W-REGISTRO.
             10 PSDCW00W-BLOCO-RETORNO.
               15 PSDCW00W-COD-RETORNO             PIC 9(02).
               15 PSDCW00W-COD-ERRO                PIC X(04).
               15 PSDCW00W-COD-MENSAGEM            PIC X(08).

