      ******************************************************************
      *                       INICIO DO BOOK                           *
      ******************************************************************
      * NOME BOOK : BVVEWT01
      * DESCRICAO : SOLICITACAO COMPOSICAO TABELAS TEMPORARIAS         *
      * DATA      : 06/01/2014                                         *
      * EMPRESA   : 7COMM                                              *
      * SISTEMA   : BVVE                                               *
      * TAMANHO   : 00386 BYTES                                        *
      *********************** DADOS DE ENTRADA**************************
      * BVVE01-CTRANS-PROG-APRES: CODIGO DE TRANSACAO DO PROGRAMA.     *
      * BVVE01-CUSUAR-REC       : USUARIO LOGADO NO TERMINAL           *
      * BVVE01-CTERM-USUAR      : CODIGO DO TERMINAL DO USUARIO.       *
      * BVVE01-IND-SEL-TABELA   : INDICADOR DE SELECAO DE TABELA.      *
      *                           DOMINIO: E - TAB.MSGEM ENVIADAS      *
      *                                    R - TAB.MSGEM DE RESPOSTAS  *
      * BVVE01-CMNTRC-MSGRA     : CHAVE DA MENSAGEM                    *
      * BVVE01-CSIST-EXTER      : CODIGO DO SITEMA DA MENSAGEM.        *
      * BVVE01-CEVNTO-MSGEM     : CODIGO DO EVENTO DA MENSAGEM.        *
      * BVVE01-CFLUXO-MSGEM     : CODIGO DO FLUXO DA MENSAGEM.         *
      * BVVE01-DMOVTO-OPER      : DATA DO MOVIMENTO.                   *
      * BVVE01-PROG-CHAMADOR    : CODIGO DO PROGRAMA CHAMADOR.         *
      * BVVE01-FILA-REENVIO     : NOME DA FILA DE REENVIO DE MENSAGEM  *
      * BVVE01-SQLCA            : CONTEUDO DO SQLCA PARA ERROS DE DB2. *
      * BVVE01-DESC-MENSAGEM    : DESCRICAO DA MENSAGEM                *
      * BVVE01-EIBTRMID         : TERMINAL DO USUARIO                  *
      * BVVE01-E-OPCAO-PESQ     : OPCAO DE PESQUISA                    *
      *                            1 - DO DIA                          *
      *                            2 - DO HISTORICO                    *
      *================================================================*
           05 BVVE01-REGISTRO.
               10 BVVE01-CTRANS-PROG-APRES         PIC X(004).
               10 BVVE01-CUSUAR-REC                PIC X(007).
               10 BVVE01-CTERM-USUAR               PIC X(015).
               10 BVVE01-IND-SEL-TABELA            PIC X(001).
               10 BVVE01-CMNTRC-MSGRA              PIC X(023).
               10 BVVE01-COD-MENSAGEM.
                   15 BVVE01-CSIST-EXTER           PIC X(003).
                   15 BVVE01-CEVNTO-MSGEM          PIC X(004).
                   15 BVVE01-CFLUXO-MSGEM          PIC X(002).
               10 BVVE01-DMOVTO-OPER               PIC X(010).
               10 BVVE01-PROG-CHAMADOR             PIC X(008).
               10 BVVE01-FILA-REENVIO              PIC X(048).
               10 BVVE01-SQLCA                     PIC X(136).
               10 BVVE01-DESC-MENSAGEM             PIC X(055).
               10 BVVE01-EIBTRMID                  PIC X(008).
               10 BVVE01-E-OPCAO-PESQ              PIC 9(001).
               10 FILLER                           PIC X(061).
      *                                                                *
      ******************************************************************
      *                          FIM DO BOOK                           *
      *****************************************************************
