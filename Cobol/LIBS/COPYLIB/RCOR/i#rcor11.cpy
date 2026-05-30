      *===============================================================*
      * INC          : I#RCOR11                                       *
      * DESCRICAO    : LAYOUT DE COMUNICAO RCOR6005, EXECUTA COMANDOS *
      *                MQ.                                            *
      * DATA CRIACAO : AGO/2012                                       *
      * AUTOR        : PRIME INFORMATICA                              *
      * TAMANHO      : 30427                                          *
      * ------------------------------------------------------------- *
      * ALTERACOES:                                                   *
      *    DATA           NOME                   DESCRICAO            *
      * ==========  =================  ============================== *
      * DD/MM/AAAA  XXXXXXXXXXXXXXXXX  XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *                                XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX *
      *===============================================================*
      *        CAMPO                       DESCRICAO                  *
      * RCOR11-A-FUNCAO            : FUNCAO A EXECUTAR NO MQ          *
      *                              O  - OPEN                        *
      *                              C  - CLOSE                       *
      *                              G  - GET                         *
      *                              GB - GET BROWSE                  *
      *                              PU - PUT UNICO                   *
      * RCOR11-A-FILA              : NOME DA FILA MQ                  *
      * RCOR11-A-HANDLE-FILA       : IDENTIFICA QUAL FILA INCIDIRA A  *
      *                              FUNCAO                           *
      * RCOR11-A-TAMANHO           : TAMANHO DA MENSAGEM              *
      * RCOR11-A-MENSAGEM          : MENSAGEM                         *
      * RCOR11-S-CD-RETORNO        : CODIGO DE RETORNO                *
      * RCOR11-S-COMPLETION-CODE   : CODIGO DE RETORNO MQ             *
      *                              0 - OK                           *
      *                              1 - WARNING                      *
      *                              2 - ERRO                         *
      * RCOR11-A-REASON-CODE       : EXPLICACAO DOS RETORNOS MQ 1 E 2 *
      *===============================================================*
       01 RCOR11-REGISTRO.
          03 RCOR11-A-FUNCAO             PIC  X(002).
          03 RCOR11-A-FILA               PIC  X(048).
          03 RCOR11-A-HANDLE-FILA        PIC  9(008) COMP.
          03 RCOR11-A-MQID               PIC  X(048).
          03 RCOR11-A-TAMANHO            PIC S9(008) COMP.
          03 RCOR11-A-MENSAGEM           PIC  X(30000).
          03 RCOR11-RETORNO.
             05 RCOR11-S-CD-RETORNO      PIC  9(004).   
             05 RCOR11-S-MENSAGEM        PIC  X(079).
             05 RCOR11-S-COMPLETION-CODE PIC  9(004).
             05 RCOR11-S-REASON-CODE     PIC  9(004).
          03 FILLER                      PIC  X(230).
