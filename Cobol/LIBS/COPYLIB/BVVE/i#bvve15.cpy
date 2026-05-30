      *---------------------------------------------------------------*
      * INC          : I#BVVE15                                       *
      * DESCRICAO    : AREA DE COMUNICACAO COM MODULO BVVE6005        *
      * DATA CRIACAO : JUNHO DE 2019.                                 *
      * AUTOR        : ELIEZER SIQUEIRA - PRIME                       *
      * TAM.REGISTRO : 100                                            *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      * DATA           NOME                   DESCRICAO               *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      * ------------------------------------------------------------- *
      *        CAMPO                       DESCRICAO                  *
      * BVVE15-E-CD-MENSAGEM   : CODIGO DA MENSAGEM ATRIBUIDO PELO    *
      *                          PROGRAMA.                            *
      * BVVE15-S-DESC-MENSAGEM : DESCRICAO DA MENSAGEM A SER RETORNADA*
      *===============================================================*
       01  BVVE15-MENSAGENS.
           05  BVVE15-E-CD-MENSAGEM              PIC 9(007).
           05  BVVE15-S-DESC-MENSAGEM            PIC X(079).
           05  FILLER                            PIC X(014).
