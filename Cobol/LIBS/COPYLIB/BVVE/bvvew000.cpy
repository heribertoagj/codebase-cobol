      ***************************************************************
      * NOME BOOK : BVVEW000 - INTERFACE ENTRE PROGRAMAS            *
      *             MODULO DE OBTENCAO DE CODIGO DE ERROS           *
      * DESCRICAO : AREA DE USO COMUM PARA RETORNO DE DADOS         *
      * DATA      : 29/06/2018                                      *
      * AUTOR     : EVANDRO                                         *
      * EMPRESA   : CAPGEMINI                                       *
      * COMPONENTE: BVVE - BRADESCO VISA SISTEMA VENDEDOR           *
      * TAMANHO    : 98    BYTES                                    *
      ***************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO         *
      ***************************************************************
      *                                                              *
      * BVVEW000-COD-LAYOUT         = CODIGO DE LAYOUT               *
      * BVVEW000-COR-TAM-LAYOUT     = TAMANHO DO LAYOUT              *
      * BVVEW000-COD-RETORNO        = CODIGO DE RETORNO              *
      * BVVEW000-COR-ERRO           = CODIGO DE ERRO                 *
      * BVVEW000-COD-MENSAGEM       = MENSAGEM DE ERRO               *
      ****************************************************************
           05 BVVEW000-HEADER.
              10 BVVEW000-COD-LAYOUT      PIC X(008) VALUE 'BVVEW000'.
              10 BVVEW000-TAM-LAYOUT      PIC 9(005) VALUE 27.
           05 BVVEW000-BLOCO-RETORNO.
              10 BVVEW000-COD-RETORNO     PIC 9(002).
              10 BVVEW000-COD-ERRO        PIC X(004).
              10 BVVEW000-COD-MENSAGEM    PIC X(008).
