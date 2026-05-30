      ****************************************************************
      * NOME BOOK.: DCOMW000 - BLOCO DE CONTROLE PARA CHAMADAS A     *
      *                        SERVICOS FUNCIONAIS                   *
      * DESCRICAO.: BLOCO DE CONTROLE PARA CHAMADAS A SERVICOS       *
      *             FUNCIONAIS E DE ACESSO A DADOS                   *
      * DATA......: 09/2017                                          *
      * AUTOR.....: PATRICK SCARAMAL     - GP 494   - WIPRO          *
      * TAMANHO...: 0027 BYTES                                       *
      ****************************************************************
      * DATA      AUTOR            DESCRICAO / MANUTENCAO            *
      ****************************************************************
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX **
      ****************************************************************
        05 DCOMW000-HEADER.
          10 DCOMW000-COD-LAYOUT        PIC  X(008)  VALUE 'DCOMW000'.
          10 DCOMW000-TAM-LAYOUT        PIC  9(005)  VALUE 027.
        05 DCOMW000-BLOCO-REGISTRO.
          10 DCOMW000-BLOCO-RETORNO.
            15 DCOMW000-COD-RETORNO     PIC  9(002).
            15 DCOMW000-COD-ERRO        PIC  X(004).
            15 DCOMW000-COD-MENSAGEM    PIC  X(008).
