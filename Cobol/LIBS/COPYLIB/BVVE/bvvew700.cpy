      ******************************************************************
      * NOME BOOK : BVVEW700                                           *
      * DESCRICAO : ATUALIZAR BASE DE MONITORAMENTO BVVE               *
      * DATA      : 31/10/2019                                         *
      * EMPRESA   : WIPRO                                              *
      * AUTOR     : CILSO GABRIEL                                      *
      * COMPONENTE: SPBN - SISTEMA DE AUDIT. DAS MENSAGENS             *
      * TAMANHO   : FIXO -   0123                                      *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    *
      ******************************************************************
       03 BVVEW700-AREA.
         05 BVVEW700-HEADER.
            10 BVVEW700-COD-LAYOUT        PIC  X(08) VALUE 'BVVEW700'.
            10 BVVEW700-TAM-LAYOUT        PIC  9(05) VALUE 0123.
          05 BVVEW700-ENTRADA.
             10 BVVEW700-E-CMNTRC-MSGRA   PIC  9(018).
          05 BVVEW700-RETORNO.
             10 BVVEW700-S-COD-RETORNO    PIC  9(004).
             10 BVVEW700-S-SQLCODE        PIC S9(004).
             10 BVVEW700-S-LOCAL          PIC  9(004).
             10 BVVEW700-S-MENSAGEM       PIC  X(080).
      ******************************************************************
