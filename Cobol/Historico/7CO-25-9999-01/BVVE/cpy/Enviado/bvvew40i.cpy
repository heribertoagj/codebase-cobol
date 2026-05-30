      ******************************************************************
      * NOME BOOK : BVVEW40I                                           *
      * DESCRICAO : TABELA  TCRDRA_PGTO_PRFIL                          *
      * DATA      : 20/08/2018                                         *
      * AUTOR     : MARIA GORETI STOPASOLLA PINTO                      *
      * EMPRESA   : BRADESCO - CD                                      *
      * COMPONENTE: BVVE                                               *
      * TAMANHO   : 430 BYTES                                          *
      **---------------------------------------------------------------*
      * DADOS DO COPYBOOK                                              *
      *----------------------------------------------------------------*
      *   BVVEW40I-E-C-ATIVIDADE = INDICATICO DE 'I' = INCLUSAO        *
      *                                          'C' = CONSULTA        *
      *                                          'A' = ALTERACAO       *
      *                                          'D' = DELECAO         *
      *   BVVEW40I-E-C-FUNCAO   = INDICATIVO DE FUNCAO DO ACESSO       *
      *        01  - RECUPERA TODAS AS INFORMACOES DA TABELA           *
      *                                                                *
      ******************************************************************
      * DATA        AUTOR            DESCRICAO / MANUTENCAO            *
      * ----------  ---------------  -------------------------------   *
      *                                                                *
      ******************************************************************
         05 BVVEW40I-HEADER.
            10 BVVEW40I-COD-LAYOUT          PIC X(008) VALUE 'BVVEW40I'.
            10 BVVEW40I-TAM-LAYOUT          PIC 9(005) VALUE 430.

         05 BVVEW40I-CONTROLE.
            10 BVVEW40I-E-C-ATIVIDADE        PIC  X(001).
            10 BVVEW40I-E-C-FUNCAO           PIC  X(003).
            10 BVVEW40I-S-C-MENSAGEM         PIC  X(079).

         05 BVVEW40I-ENTRADA.
            10 BVVEW40I-E-CCRDRA-ESTBL-COML  PIC  9(004).
7C2511*     10 BVVEW40I-E-CCNPJ-CRDRA-ESTBL  PIC  9(009).
7C2511      10 BVVEW40I-E-CCNPJ-CRDRA-ESTBL-ST  PIC  X(009).
7C2511*     10 BVVEW40I-E-CFLIAL-CRDRA-ESTBL PIC  9(004).
7C2511      10 BVVEW40I-E-CFLIAL-CRDRA-ESTBL-ST PIC  X(004).
            10 BVVEW40I-E-CCTRL-CRDRA-ESTBL  PIC  9(002).
            10 BVVEW40I-E-IREDZD-CRDRA-ESTBL PIC  X(030).
            10 BVVEW40I-E-ICRDRA-ESTBL-COML  PIC  X(050).
            10 BVVEW40I-E-HINCL-REG          PIC  X(026).
            10 BVVEW40I-E-CUSUAR-INCL        PIC  X(009).
            10 BVVEW40I-E-HMANUT-REG         PIC  X(026).
            10 BVVEW40I-E-CUSUAR-MANUT       PIC  X(007).

         05 BVVEW40I-SAIDA.
            10 BVVEW40I-S-CCRDRA-ESTBL-COML  PIC  9(004).
7C2511*     10 BVVEW40I-S-CCNPJ-CRDRA-ESTBL  PIC  9(009).
7C2511      10 BVVEW40I-S-CCNPJ-CRDRA-ESTBL-ST  PIC  X(009).
7C2511*     10 BVVEW40I-S-CFLIAL-CRDRA-ESTBL PIC  9(004).
7C2511      10 BVVEW40I-S-CFLIAL-CRDRA-ESTBL-ST PIC  X(004).
            10 BVVEW40I-S-CCTRL-CRDRA-ESTBL  PIC  9(002).
            10 BVVEW40I-S-IREDZD-CRDRA-ESTBL PIC  X(030).
            10 BVVEW40I-S-ICRDRA-ESTBL-COML  PIC  X(050).
            10 BVVEW40I-S-HINCL-REG          PIC  X(026).
            10 BVVEW40I-S-CUSUAR-INCL        PIC  X(009).
            10 BVVEW40I-S-HMANUT-REG         PIC  X(026).
            10 BVVEW40I-S-CUSUAR-MANUT       PIC  X(007).

      ******************************************************************
      *****                    FIM DO BOOK BVVEW40I                 ****
      ******************************************************************