      ******************************************************************
      * NOME BOOK : CEPNW00W - BOOK DE CONTROLE A ACESSO A FUNCIONAIS
      * DESCRICAO :
      * DATA      : 10/06/2010
      * AUTOR     : MARCUS CAVALCANTI
      * EMPRESA   : CPM BRAXIS
      * GRUPO     : CPM BRAXIS
      * COMPONENTE:
      * TAMANHO   : 27 BYTES
      ************************ DADOS DE RETORNO ***********************
      * CEPNW00W-COD-RETORNO         = CODIGO DE RETORNO
      * CEPNW00W-COD-ERRO            = CODIGO DE ERRO
      * CEPNW00W-COD-MENSAGEM        = CODIGO DE MENSAGEM
      *
      *****************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO
      ****************************************************************
           10 CEPNW00W-COD-LAYOUT     PIC X(008)  VALUE 'CEPNW00W'.
           10 CEPNW00W-TAM-LAYOUT     PIC 9(005)  VALUE 27.
         05 CEPNW00W-REGISTRO.
           10 CEPNW00W-BLOCO-RETORNO.
              15 CEPNW00W-COD-RETORNO        PIC 9(002).
              15 CEPNW00W-COD-ERRO           PIC X(004).
              15 CEPNW00W-COD-MENSAGEM       PIC X(008).

