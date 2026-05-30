      *---------------------------------------------------------------*
      * INC          : I#BVVE14                                       *
      * DESCRICAO    : MENSAGENS DE ERRO PARA O SISTEMA BVVE          *
      * DATA CRIACAO : JUNHO DE 2019.                                 *
      * AUTOR-       : ELIEZER SIQUEIRA                               *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      * ------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                  *
      * BVVE14-COD-MENSAGEM    : CODIGO DA MENSAGEM ATRIBUIDOPELO     *
      *                          PROGRAMA.                            *
      * BVVE14-DESC-MENSAGEM   : DESCRICAO DA MENSAGEM A SER          *
      *                          APRESENTADA AO USUARIO               *
      * BVVE14-QTDE-MENSAGENS  : QUANTIDADE DE MENSAGENS CADASTRADAS  *
      *                          NA INCLUDE.                          *
      * OBS : QUANDO FOR ALTERAR A INCLUDE COM A INCLUSAO OU EXCLUSAO *
      *       DE ALGUMA OCORRENCIA, DEVE SER ALTERADA A CLAUSULA      *
      *       OCCURS E IMPORTANTE ALTERAR O CAMPO                     *
      *       BVVE14-QTDE-MENSAGENS, POIS O MESMO SERA O CONTROLE DE  *
      *       PESQUISA DOS PROGRAMAS QUE A UTILIZAM.                  *
      *===============================================================*
       01  BVVE14-MENSAGENS-ERRO.
           05  BVVE14-TAB-MENSAGENS.
               10  FILLER                         PIC X(082) VALUE
                   '001CAMPO(S) OBRIGATORIO(S)'.
               10  FILLER                         PIC X(082) VALUE
                   '002CAMPO(S) INCONSISTENTE(S)'.
               10  FILLER                         PIC X(082) VALUE
                   '003REGISTRO EXISTENTE'.
               10  FILLER                         PIC X(082) VALUE
                   '004REGISTRO INEXISTENTE'.
               10  FILLER                         PIC X(082) VALUE
                   '005ACESSO NEGADO'.
               10  FILLER                         PIC X(082) VALUE
                   '006AMOSTRAGEM CONTINUA'.
                   '007FIM DA AMOSTRAGEM'.
               10  FILLER                         PIC X(082) VALUE
                   '008FASE INVALIDA'.
               10  FILLER                         PIC X(082) VALUE
                   '009CONSULTA REALIZADA COM SUCESSO'.
               10  FILLER                         PIC X(082) VALUE
                   '010ALTERACAO REALIZADA COM SUCESSO'.
               10  FILLER                         PIC X(082) VALUE
                   '011EXCLUSAO REALIZADA COM SUCESSO'.
               10  FILLER                         PIC X(082) VALUE
                   '012PF INVALIDA'.
               10  FILLER                         PIC X(082) VALUE
                   '013ALTERE OS DADOS E TECLE <ENTER>'.
               10  FILLER                         PIC X(082) VALUE
                   '014TECLE <PF6> PARA CONFIRMAR'.
               10  FILLER                         PIC X(082) VALUE
                   '015TECLE <PF6> PARA CONFIRMAR A DESATIVACAO DO PRODU
      -            'TO'.
               10  FILLER                         PIC X(082) VALUE
                   '016PRODUTO JA DESATIVADO'.
                   '017SELECIONE UMA OPCAO COM "D"ETALHE, "A"LTERAR OU
      -            '"E"XCLUIR E TECLE <ENTER>'.
               10  FILLER                         PIC X(082) VALUE
                   '018SELECIONE APENAS UMA LINHA'.
               10  FILLER                         PIC X(082) VALUE
                   '019NAO HA MAIS PAGINAS PARA AVANCAR'.
               10  FILLER                         PIC X(082) VALUE
                   '020NAO HA MAIS PAGINAS PARA VOLTAR'.
               10  FILLER                         PIC X(082) VALUE
                   '021INFORME A OPCAO DESEJADA E TECLE <ENTER>'.
               10  FILLER                         PIC X(082) VALUE
                   '022DIGITE OS DADOS E TECLE <ENTER>'.
               10  FILLER                         PIC X(082) VALUE
                   '023TECLE <PF6> PARA INCLUIR O REGISTRO'.
               10  FILLER                         PIC X(082) VALUE
                   '024INCLUSAO REALIZADA COM SUCESSO. TECLE <ENTER> PAR
      -            'A INCLUIR NOVA BANDEIRA'.
               10  FILLER                         PIC X(082) VALUE
                   '025BANDEIRA INEXISTENTE NA BASE CORPORATIVA'.
               10  FILLER                         PIC X(082) VALUE
               10  FILLER                         PIC X(082) VALUE
                   '027BANDEIRA EXCLUIDA POR OUTRO USUARIO'.
               10  FILLER                         PIC X(082) VALUE
                   '028REGISTRO EXISTENTE'.
               10  FILLER                         PIC X(082) VALUE
                   '029CREDENCIADORA INEXISTENTE NA BASE PRINCIPAL'.
               10  FILLER                         PIC X(082) VALUE
                   '030PRODUTO EXCLUIDO DA BASE'.
               10  FILLER                         PIC X(082) VALUE
                   '031PRODUTO DESATIVADO COM SUCESSO'.
               10  FILLER                         PIC X(082) VALUE
                   '032INCLUSAO REALIZADA COM SUCESSO. TECLE <ENTER> PAR
      -            'A INCLUIR NOVO PRODUTO'.
               10  FILLER                         PIC X(082) VALUE
                   '033NAO HA BANDEIRA PARA LISTAGEM'.
               10  FILLER                         PIC X(082) VALUE
                   '034ERRO AO LISTAR BANDEIRA'.
               10  FILLER                         PIC X(082) VALUE
                   '035SELECIONE UMA BANDEIRA COM "X" E TECLE <PF3>'.
               10  FILLER                         PIC X(082) VALUE
               10  FILLER                         PIC X(082) VALUE
                   '037SELECIONE UMA OPCAO COM "D"ETALHE OU "A"LTERAR E
      -            'TECLE <ENTER>'.
           05  BVVE14-TAB-MENSAGENS-R  REDEFINES  BVVE14-TAB-MENSAGENS
                      OCCURS 037 TIMES INDEXED BY IND-MSG.
               10  BVVE14-COD-MENSAGEM            PIC 9(003).
               10  BVVE14-DESC-MENSAGEM           PIC X(079).
           05  BVVE14-QTDE-MENSAGENS              PIC 9(003) VALUE 037.
