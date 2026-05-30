      ******************************************************************
      * NOME BOOK : BVVEWCLI                                           *
      * DESCRICAO : TABELA                                             *
      * DATA      : 09/09/2018                                         *
      * AUTOR     : MARIA GORETI STOPASOLLA PINTO                      *
      * EMPRESA   : BRADESCO - CD                                      *
      * COMPONENTE: BVVE                                               *
      * TAMANHO   : 123 BYTES                                          *
      *----------------------------------------------------------------*
      * DADOS DO COPYBOOK                                              *
      *----------------------------------------------------------------*
      *   BVVEWCLI-E-C-ATIVIDADE = INDICATICO DE 'C' = CONSULTA        *
      *   BVVEWCLI-E-C-FUNCAO    = INDICATIVO DE FUNCAO DO ACESSO      *
      *        01  - RECUPERA TODAS AS INFORMACOES DA TABELA           *
      *                                                                *
      ******************************************************************
      * DATA        AUTOR            DESCRICAO / MANUTENCAO            *
      * ----------  ---------------  -------------------------------   *
      *                                                                *
      ******************************************************************
         05 BVVEWCLI-HEADER.
            10 BVVEWCLI-COD-LAYOUT          PIC X(008) VALUE 'BVVEWCLI'.
            10 BVVEWCLI-TAM-LAYOUT          PIC 9(005) VALUE 123.

         05 BVVEWCLI-CONTROLE.
            10 BVVEWCLI-E-C-ATIVIDADE       PIC  X(001).
            10 BVVEWCLI-E-C-FUNCAO          PIC  X(003).

         05 BVVEWCLI-ENTRADA.
            10 BVVEWCLI-E-CJUNC-DEPDC       PIC  9(005).
            10 BVVEWCLI-E-CCTA-CLI          PIC  9(007).
            10 BVVEWCLI-E-CID-CLI           PIC  X(026).
            10 BVVEWCLI-E-CCGC-CPF          PIC  9(009).
            10 BVVEWCLI-E-CFLIAL-CGC        PIC  9(004).
            10 BVVEWCLI-E-CCTRL-CPF-CGC     PIC  9(002).

         05 BVVEWCLI-SAIDA.
            10 BVVEWCLI-S-CJUNC-DEPDC       PIC  9(005).
            10 BVVEWCLI-S-CCTA-CLI          PIC  9(007).
            10 BVVEWCLI-S-CID-CLI           PIC  X(026).
            10 BVVEWCLI-S-CCGC-CPF          PIC  9(009).
            10 BVVEWCLI-S-CFLIAL-CGC        PIC  9(004).
            10 BVVEWCLI-S-CCTRL-CPF-CGC     PIC  9(002).

      ******************************************************************
      *****                    FIM DO BOOK BVVEWCLI                 ****
      ******************************************************************
