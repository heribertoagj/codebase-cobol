      *---------------------------------------------------------------*
      * INC          : I#BVVEER                                       *
      * DESCRICAO    : FORMATACAO DE MENSAGENS PADRAO PARA ERROS DB2, *
      *                SUB-ROTINA E CICS.                             *
      * DATA CRIACAO : JUNHO DE 2019.                                 *
      * AUTOR-       : ELIEZER SIQUEIRA                               *
      * TAM.REGISTRO : 237                                            *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      * ------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                  *
      * BVVEER-CD-PRO-CHAM-SUBRO-CICS: NOME DO PROGRAMA CHAMADOR DA   *
      *                                SUB-ROTINA COM ERRO CICS.      *
      * BVVEER-CD-SUBROT-CICS  : NOME DA SUB-ROTINA EM QUE OCORREU O  *
      *                          ERRO CICS.                           *
      * BVVEER-CD-RET-SUBROT-CICS: CODIGO DE ERRO CICS NA SUB-ROTINA  *
      * BVVEER-TX-MSG-SUBROT-CICS   : TEXTO DA MENSAGEM DE ERRO CICS  *
      *                               NA SUB-ROTINA.                  *
      * BVVEER-LOCAL-SUBROT-CICS    : LOCAL NO PROGRAMA CHAMADOR DA   *
      *                               SUB-ROTINA CICS.                *
      * BVVEER-CD-PROG-CHAMA-SUBROT : NOME DO PROGRAMA CHAMADOR DA    *
      *                               SUB-ROTINA COM ERRO.            *
      * BVVEER-CD-SUBROT       : NOME DA SUB-ROTINA EM QUE OCORREU O  *
      *                          ERRO.                                *
      * BVVEER-CD-RET-SUBROT   : CODIGO DE ERRO DA SUB-ROTINA.        *
      * BVVEER-TX-MSG-SUBROT   : TEXTO DA MENSAGEM DE ERRO DA SUB-    *
      *                          ROTINA.                              *
      * BVVEER-LOCAL-SUBROT    : LOCAL NO PROGRAMA CHAMADOR DA SUB-   *
      *                          ROTINA COM ERRO.                     *
      * BVVEER-CD-PROG-CHAMA-DB2: NOME DO PROGRAMA CHAMADOR DA SUB-   *
      *                           ROTINA COM ERRO DB2 OU PROGRAMA     *
      *                           PRINCIPAL, CASO NAO CHAME SUB-ROTINA*
      * BVVEER-CD-SUBROT-DB2   : NOME DA SUB-ROTINA EM QUE OCORREU O  *
      *                          ERRO DB2.                            *
      * BVVEER-CD-TABELA-DB2   : NOME DA TABELA DB2 COM ERRO.         *
      * BVVEER-SINAL-SQLCODE   : SINAL DO CODIGO SQLCODE.             *
      * BVVEER-SQLCODE-DB2     : CODIGO DE RETORNO DO DB2.            *
      * BVVEER-NM-COMANDO-DB2  : COMANDO EXECUTADO NO MOMENTO DO ERRO *
      *                          DB2.                                 *
      * BVVEER-LOCAL-DB2       : LOCAL DO ERRO DB2 NA SUB-ROTINA OU   *
      *                          NO PROGRAMA PRINCIPAL.               *
      *===============================================================*
       01 BVVEER-MENSAGEM.
           05 BVVEER-MSG-SUBROT-CICS.
              10 BVVEER-CD-PRO-CHAM-SUBRO-CICS PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X      VALUE
                 '-'.
              10 BVVEER-CD-SUBROT-CICS         PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X(004) VALUE
                 '/RT '.
              10 BVVEER-CD-RET-SUBROT-CICS     PIC 9(004) VALUE ZEROS.
              10 FILLER                        PIC X      VALUE
                 '-'.
              10 BVVEER-TX-MSG-SUBROT-CICS     PIC X(046) VALUE SPACES.
              10 FILLER                        PIC X(005) VALUE
                 '/LOC-'.
              10 BVVEER-LOCAL-SUBROT-CICS      PIC 9(002) VALUE ZEROS.
           05 BVVEER-MSG-SUBROT.
              10 BVVEER-CD-PROG-CHAMA-SUBROT   PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X      VALUE
                 '-'.
              10 BVVEER-CD-SUBROT              PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X(004) VALUE
                 '/RT '.
              10 BVVEER-CD-RET-SUBROT          PIC 9(004) VALUE ZEROS.
              10 FILLER                        PIC X      VALUE
                 '-'.
              10 BVVEER-TX-MSG-SUBROT          PIC X(046) VALUE SPACES.
              10 FILLER                        PIC X(005) VALUE
                 '/LOC-'.
              10 BVVEER-LOCAL-SUBROT           PIC 9(002) VALUE ZEROS.
           05 BVVEER-MSG-DB2.
              10 BVVEER-CD-PROG-CHAMA-DB2      PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X      VALUE
                 '-'.
              10 BVVEER-CD-SUBROT-DB2          PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X(014) VALUE
                 '/ERRO DB2 TB- '.
              10 BVVEER-CD-TABELA-DB2          PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X(010) VALUE
                 '/SQLCODE- '.
              10 BVVEER-SINAL-SQLCODE          PIC X      VALUE SPACES.
              10 BVVEER-SQLCODE-DB2            PIC 9(003) VALUE ZEROS.
              10 FILLER                        PIC X(010) VALUE
                 '/COMANDO- '.
              10 BVVEER-NM-COMANDO-DB2         PIC X(008) VALUE SPACES.
              10 FILLER                        PIC X(006) VALUE
                 '/LOC- '.
              10 BVVEER-LOCAL-DB2              PIC 9(002) VALUE ZEROS.
