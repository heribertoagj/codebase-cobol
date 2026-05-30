      ***************************************************************
      * NOME BOOK : CMTFW000                                        *
      * DESCRICAO : BOOK PADRAO FRAMEWORK                           *
      * DATA      : DEZEMBRO/2011                                   *
      * AUTOR     : ANDRE FERNANDES VIEIRA                          *
      * EMPRESA   : SONDAPROCWORK                                   *
      * GRUPO     : CMTF                                            *
      * TAMANHO   : 027                                             *
      ***************************************************************
      * DESCRICAO DOS CAMPOS                                        *
      ***************************************************************
      * CMTFW000-COD-LAYOUT          -> CODIGO LAYOUT               *
      * CMTFW000-TAM-LAYOUT          -> TAMANHO LAYOUT              *
      * CMTFW000-COD-RETORNO         -> CODIGO RETORNO              *
      * CMTFW000-COD-ERRO            -> CODIGO ERRO                 *
      * CMTFW000-COD-MENSAGEM        -> CODIGO MENSAGEM             *
      ***************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO         *
      ***************************************************************
       05  CMTFW000-HEADER.
               10 CMTFW000-COD-LAYOUT  PIC X(08) VALUE 'CMTFW000'.
               10 CMTFW000-TAM-LAYOUT  PIC 9(05) VALUE 27.
       05  CMTFW000-BLOCO-RETORNO.
               10 CMTFW000-COD-RETORNO  PIC 9(002).
               10 CMTFW000-COD-ERRO     PIC X(004).
               10 CMTFW000-COD-MENSAGEM PIC X(008).
