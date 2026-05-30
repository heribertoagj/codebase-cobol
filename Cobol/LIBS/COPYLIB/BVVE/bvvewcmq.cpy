      *----------------------------------------------------------------*
      * INC          : BVVEWCMQ                                        *
      * DESCRICAO    : LAYOUT DE COMUNICACAO COM O MÓDULO BVVE5007 -   *
      *                DE MQ                                           *
      * DATA CRIACAO : 08/2020                                         *
      * AUTOR        : KLEBER SANTOS        -  7COMM INFORMATICA       *
      * TAM.REGISTRO : 30500                                           *
      * -------------------------------------------------------------  *
      * ALTERACOES:                                                    *
      *    DATA           NOME                   DESCRICAO             *
      * ==========  =================  ==============================  *
      * -------------------------------------------------------------  *
      *        CAMPO                       DESCRICAO                   *
      * BVVECMQ-A-FUNCAO              : FUNCAO A EXECUTAR NO MQ        *
      *                                       O  - OPEN                *
      *                                       C  - CLOSE               *
      *                                       G  - GET                 *
      *                                       GB - GET BROWSE          *
      *                                       P  - PUT                 *
      *                                       PU - PUT UNICO           *
      * BVVECMQ-A-FILA                : NOME DA FILA MQ                *
      * BVVECMQ-A-HANDLE-FILA         : IDENTIFICA QUAL FILA INCIDIRA A*
      *                                 FUNCAO                         *
      * BVVECMQ-A-TAMANHO             : TAMANHO DA MENSAGEM            *
      * BVVECMQ-A-BUFFER              : AGRUPADO DE BLOCOS             *
      * BVVECMQ-S-CD-RETORNO          : CODIGO DE RETORNO              *
      * BVVECMQ-S-MENSAGEM            : MENSAGEM DE ERRO               *
      * BVVECMQ-S-SQLCA               : RETORNO DE ERRO DB2            *
      * BVVECMQ-S-COUNT-BACKOUT       : INFORMA A QUANTAS VEZES QUE A  *
      *                                 A MENSAGEM VOLTOU PARA A FILA  *
      *================================================================*
       01 BVVECMQ-REGISTRO.
          03 BVVECMQ-A-FUNCAO                 PIC  X(002).
          03 BVVECMQ-A-FILA                   PIC  X(048).
          03 BVVECMQ-A-HANDLE-FILA            PIC  9(008) COMP.
          03 BVVECMQ-A-TAMANHO                PIC S9(008) COMP.
          03 BVVECMQ-A-BUFFER                 PIC  X(30000).
          03 BVVECMQ-RETORNO.
             05 BVVECMQ-S-CD-RETORNO          PIC  9(004).
             05 BVVECMQ-S-MENSAGEM            PIC  X(079).
             05 BVVECMQ-S-SQLCA               PIC  X(136).
          03 BVVECMQ-S-COUNT-BACKOUT          PIC  9(003).
          03 BVVECMQ-S-MQ.
             05 BVVECMQ-S-MQ-ID-MENSAGEM      PIC  X(024).
             05 BVVECMQ-S-MQ-FILA-RESPOSTA    PIC  X(048).
             05 BVVECMQ-S-MQ-QM-RESPOSTA      PIC  X(016).
          03 FILLER                           PIC  X(132).
