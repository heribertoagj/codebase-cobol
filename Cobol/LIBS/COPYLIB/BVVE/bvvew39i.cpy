      ******************************************************************
      * NOME BOOK : BVVEW39I                                           *
      * DESCRICAO : TABELA  TCCRDRA-ARRJO-PGTO-LCTO                    *
      * DATA      : 20/08/2018                                         *
      * AUTOR     : MARIA GORETI STOPASOLLA PINTO                      *
      * EMPRESA   : BRADESCO - CD                                      *
      * COMPONENTE: BVVE                                               *
      * TAMANHO   : 318 BYTES                                          *
      *----------------------------------------------------------------*
      * DADOS DO COPYBOOK                                              *
      *----------------------------------------------------------------*
      *   BVVEW39I-E-C-ATIVIDADE = INDICATICO DE 'I' = INCLUSAO        *
      *                                          'C' = CONSULTA        *
      *                                          'A' = ALTERACAO       *
      *                                          'D' = DELECAO         *
      *   BVVEW39I-E-C-FUNCAO   = INDICATIVO DE FUNCAO DO ACESSO       *
      *        01  - RECUPERA TODAS AS INFORMACOES DA TABELA           *
      *                                                                *
      ******************************************************************
      * DATA        AUTOR            DESCRICAO / MANUTENCAO            *
      * ----------  ---------------  -------------------------------   *
      *                                                                *
      ******************************************************************
         05 BVVEW39I-HEADER.
            10 BVVEW39I-COD-LAYOUT          PIC X(008) VALUE 'BVVEW39I'.
            10 BVVEW39I-TAM-LAYOUT          PIC 9(005) VALUE 318.

         05 BVVEW39I-CONTROLE.
            10 BVVEW39I-E-C-ATIVIDADE        PIC  X(001).
            10 BVVEW39I-E-C-FUNCAO           PIC  X(003).
            10 BVVEW39I-S-C-MENSAGEM         PIC  X(079).

         05 BVVEW39I-ENTRADA.
            10 BVVEW39I-E-CCRDRA-ESTBL-COML  PIC  9(004).
            10 BVVEW39I-E-CARRJO-PGTO        PIC  9(003).
            10 BVVEW39I-E-NPRFIL-CRDRA-PGTO  PIC  9(001).
            10 BVVEW39I-E-CIDTFD-LCTO        PIC  9(005).
            10 BVVEW39I-E-HINCL-REG          PIC  X(026).
            10 BVVEW39I-E-CUSUAR-INCL        PIC  X(009).
            10 BVVEW39I-E-HMANUT-REG         PIC  X(026).
            10 BVVEW39I-E-CUSUAR-MANUT       PIC  X(007).

         05 BVVEW39I-SAIDA.
            10 BVVEW39I-S-CCRDRA-ESTBL-COML  PIC  9(004).
            10 BVVEW39I-S-IREDZD-CRDRA-ESTBL PIC  X(030).
            10 BVVEW39I-S-CARRJO-PGTO        PIC  9(003).
            10 BVVEW39I-S-IARRJO-PGTO        PIC  X(030).
            10 BVVEW39I-S-NPRFIL-CRDRA-PGTO  PIC  9(001).
            10 BVVEW39I-S-CIDTFD-LCTO        PIC  9(005).
            10 BVVEW39I-S-HINCL-REG          PIC  X(026).
            10 BVVEW39I-S-CUSUAR-INCL        PIC  X(009).
            10 BVVEW39I-S-HMANUT-REG         PIC  X(026).
            10 BVVEW39I-S-CUSUAR-MANUT       PIC  X(007).

      ******************************************************************
      *****                    FIM DO BOOK BVVEW39I                 ****
      ******************************************************************
