      ***************************************************************
      * NOME BOOK : GFCTW100                                        *
      * DESCRICAO : BOOK PADRAO FRAMEWORK                           *
      * DATA      : JUNHO/2013                                      *
      * AUTOR     : ANDRE FERNANDES VIEIRA                          *
      * EMPRESA   : SONDAPROCWORK                                   *
      * GRUPO     : GFCT                                            *
      * TAMANHO   : 027                                             *
      ***************************************************************
      * DESCRICAO DOS CAMPOS                                        *
      ***************************************************************
      * GFCTW100-COD-LAYOUT          -> CODIGO LAYOUT               *
      * GFCTW100-TAM-LAYOUT          -> TAMANHO LAYOUT              *
      * GFCTW100-COD-RETORNO         -> CODIGO RETORNO              *
      * GFCTW100-COD-ERRO            -> CODIGO ERRO                 *
      * GFCTW100-COD-MENSAGEM        -> CODIGO MENSAGEM             *
      ***************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO         *
      ***************************************************************
       05  GFCTW100-HEADER.
               10 GFCTW100-COD-LAYOUT   PIC X(08) VALUE 'GFCTW100'.
               10 GFCTW100-TAM-LAYOUT   PIC 9(05) VALUE 27.
       05  GFCTW100-BLOCO-RETORNO.
               10 GFCTW100-COD-RETORNO  PIC 9(002).
               10 GFCTW100-COD-ERRO     PIC X(004).
               10 GFCTW100-COD-MENSAGEM PIC X(008).
      ******************************************************************
      *                        FINAL DO BOOK                           *
      ******************************************************************
