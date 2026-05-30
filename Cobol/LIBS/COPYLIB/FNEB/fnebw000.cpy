      ******************************************************************
      * NOME BOOK : FNEBW000 - BOOK DE CONTROLE A ACESSO A FUNCIONAIS  *
      * DESCRICAO : BOOK PADRAO DE MODULOS FUNCIONAIS                  *
      * DATA      : 04/03/2010                                         *
      * AUTOR     : MATHEUS MAGALHAES                                  *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : CPM BRAXIS                                         *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 27 BYTES                                           *
      ************************ DADOS DE RETORNO ************************
      * FNEBW000-COD-RETORNO         = CODIGO DE RETORNO               *
      * FNEBW000-COD-ERRO            = CODIGO DE ERRO                  *
      * FNEBW000-COD-MENSAGEM        = CODIGO DE MENSAGEM              *
      *                                                                *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 FNEBW000-HEADER.
             10 FNEBW000-COD-LAYOUT    PIC X(008)  VALUE 'FNEBW000'.
             10 FNEBW000-TAM-LAYOUT    PIC 9(005)  VALUE 27.
           05 FNEBW000-REGISTRO.
             10 FNEBW000-BLOCO-RETORNO.
               15 FNEBW000-COD-RETORNO PIC 9(002).
               15 FNEBW000-COD-ERRO    PIC X(004).
               15 FNEBW000-COD-MENSAGEM
                                       PIC X(008).
