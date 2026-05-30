      *===============================================================*
      * INC          : BVVEW0BK                                       *
      * DESCRICAO    : COMPOR LINHAS NAS TABELAS TEMPORARIAS          *
      * DATA CRIACAO : 09/01/2014                                     *
      * TAM.REGISTRO : 28900                                          *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      *                                                               *
      * ------------------------------------------------------------- *
      *    CAMPO                               DESCRICAO              *
      *    =====                               ==========             *
      * BVVEBK-CSIST-EXTER     : CODIGO DO SITEMA DA MENSAGEM.        *
      * BVVEBK-CEVNTO-MSGEM    : CODIGO DO EVENTO DA MENSAGEM.        *
      * BVVEBK-CFLUXO-MSGEM    : CODIGO DO FLUXO DA MENSAGEM.         *
      * BVVEBK-DMOVTO-OPER     : DATA DO MOVIMENTO.                   *
      * BVVEBK-NORD-CPO-MSGEM  : NUMERO DE ORDEM DO CAMPO NA MENSAGEM.*
      * BVVEBK-NORD-NVEL       ; NIVEL DE REPETICAO.                  *
      * BVVEBK-ICPO-MSGRA      : NOME DA TAG.                         *
      * BVVEBK-RCPO-MSGEM-MSGRA: NOME DO CAMPO                        *
      * BVVEBK-CINDCD-OBRIG    : INDICADOR DE CAMPO OBRIGATORIO.      *
      *                          DOMINIO: S - CAMPO OBRIGATORIO       *
      *                                   N - CAMPO NAO OBRIGATORIO   *
      * BVVEBK-CINDCD-FORMT    : INDICADOR DO FORMATO DO CAMPO.       *
      *                          DOMINIO: X - FORMATO ALFANUMERICO    *
      *                                   N - FORMATO NUMERICO        *
      * BVVEBK-NTMNHO-CPO      : TAMANHO DO CAMPO.                    *
      * BVVEBK-QCASA-DEC       : QUANTIDADE DE CASAS DECIMAIS.        *
      * BVVEBK-QREPTC-MSGEM    : QUANTIDADE DE REPETICOES QUANDO O    *
      *                          CAMPO FOR DE REPETICAO.              *
      * BVVEBK-CAMPO-NVEL-ANT  : INDICA QUE O CAMPO E' PERTENCENTE A  *
      *                          NIVEL ANTERIOR.                      *
      *                          DOMINIO: BRANCO - NAO PERTENCE       *
      *                                   S      - PERTENCE           *
      * BVVEBK-PROG-CHAMADOR   : CODIGO DO PROGRAMA CHAMADOR.         *
      * BVVEBK-SQLCA           : CONTEUDO DO SQLCA PARA ERROS DE DB2. *
      * BVVEBK-DESC-MENSAGEM   : DESCRICAO DA MENSAGEM                *
      * BVVEBK-CD-RETORNO      : CODIGO DE RETORNO.                   *
      * BVVEBK-MENSAGEM        : MENSAGEM DE RETORNO.                 *
      *===============================================================*
           05 BVVEBK-REGISTRO.
              10 BVVEBK-DADOS-SELECAO.
                 15 BVVEBK-COD-MENSAGEM.
                    20 BVVEBK-CSIST-EXTER                   PIC X(003).
                    20 BVVEBK-CEVNTO-MSGEM                  PIC X(004).
                    20 BVVEBK-CFLUXO-MSGEM                  PIC X(002).
                 15 BVVEBK-DMOVTO-OPER                      PIC X(010).
              10 BVVEBK-DADOS-RETORNO.
                 15 BVVEBK-DADOS-TABELA OCCURS 300.
                    20 BVVEBK-NORD-CPO-MSGEM                PIC 9(003).
                    20 BVVEBK-NORD-NVEL                     PIC 9(004).
                    20 BVVEBK-ICPO-MSGRA                    PIC X(040).
                    20 BVVEBK-RCPO-MSGEM-MSGRA              PIC X(036).
                    20 BVVEBK-CINDCD-OBRIG                  PIC X.
                    20 BVVEBK-CINDCD-FORMT                  PIC X.
                    20 BVVEBK-NTMNHO-CPO                    PIC 9(004).
                    20 BVVEBK-QCASA-DEC                     PIC 9(002).
                    20 BVVEBK-QREPTC-MSGEM                  PIC 9(003).
                    20 BVVEBK-CAMPO-NVEL-ANT                PIC X.
              10 BVVEBK-PROG-CHAMADOR                       PIC X(008).
              10 BVVEBK-DESC-MENSAGEM                       PIC X(055).
              10 FILLER                                     PIC X(100).
