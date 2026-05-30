      ******************************************************************
      * NOME BOOK : BVVEW41I                                           *
      * DESCRICAO : TABELA  TCCRDRA_PGTO_PRFIL                         *
      * DATA      : 17/08/2018                                         *
      * AUTOR     : MARIA GORETI STOPASOLLA PINTO                      *
      * EMPRESA   : BRADESCO - CD                                      *
      * COMPONENTE: BVVE                                               *
      * TAMANHO   : 398 BYTES                                          *
      *----------------------------------------------------------------*
      * DADOS DO COPYBOOK                                              *
      *----------------------------------------------------------------*
      *   BVVEW41I-E-C-ATIVIDADE = INDICATICO DE 'I' = INCLUSAO        *
      *                                          'C' = CONSULTA        *
      *                                          'A' = ALTERACAO       *
      *                                          'D' = DELECAO         *
      *   BVVEW41I-E-C-FUNCAO   = INDICATIVO DE FUNCAO DO ACESSO       *
      *        01  - RECUPERA INFORMACOES BANCARIAS                    *
      *   BVVEW41I-E-C-OPCAO    = INDICATIVO DE QUE TIPO DE CONSULTA/  *
      *                           ALTERACAO/DELECAO                    *
      *        01  - PARA CONSULTA TODOS OS CAMPOS                     *
      *                                                                *
      ******************************************************************
      * DATA        AUTOR            DESCRICAO / MANUTENCAO            *
      * ----------  ---------------  -------------------------------   *
      *                                                                *
      ******************************************************************
           05 BVVEW41I-HEADER.
               10 BVVEW41I-COD-LAYOUT      PIC X(008) VALUE 'BVVEW41I'.
               10 BVVEW41I-TAM-LAYOUT      PIC 9(005) VALUE  400.
    ********   10 BVVEW41I-TAM-LAYOUT      PIC 9(005) VALUE  398.

           05 BVVEW41I-CONTROLE.
               10 BVVEW41I-E-C-ATIVIDADE        PIC  X(001).
               10 BVVEW41I-E-C-FUNCAO           PIC  X(003).
               10 BVVEW41I-COD-MENSAGEM         PIC  X(079).

           05 BVVEW41I-ENTRADA.
               10 BVVEW41I-E-CCRDRA-ESTBL-COML  PIC  9(004).
               10 BVVEW41I-E-CCNPJ-CRDRA-ESTBL  PIC  9(009).
               10 BVVEW41I-E-CFLIAL-CRDRA-ESTBL PIC  9(004).
               10 BVVEW41I-E-CCTRL-CRDRA-ESTBL  PIC  9(002).
               10 BVVEW41I-E-IREDZD-CRDRA-ESTBL PIC  X(030).
               10 BVVEW41I-E-NPRFIL-CRDRA-PGTO  PIC  9(001).
               10 BVVEW41I-E-NSEQ-CONTR-NEGOC   PIC  9(010).
               10 BVVEW41I-E-CBCO               PIC  9(003).
               10 BVVEW41I-E-CAG-BCRIA-CLI      PIC  9(005).
               10 BVVEW41I-E-NCTA-BCRIA         PIC  9(012).
               10 BVVEW41I-E-CTPO-CONTR-CTA     PIC  9(003).
               10 BVVEW41I-E-HINCL-REG          PIC  X(026).
               10 BVVEW41I-E-CUSUAR-INCL        PIC  X(009).
               10 BVVEW41I-E-HMANUT-REG         PIC  X(026).
               10 BVVEW41I-E-CUSUAR-MANUT       PIC  X(007).
               10 BVVEW41I-E-FASE               PIC  9(001).

           05 BVVEW41I-SAIDA.
               10 BVVEW41I-S-CCRDRA-ESTBL-COML  PIC  9(004).
               10 BVVEW41I-S-CCNPJ-CRDRA-ESTBL  PIC  9(009).
               10 BVVEW41I-S-CFLIAL-CRDRA-ESTBL PIC  9(004).
               10 BVVEW41I-S-CCTRL-CRDRA-ESTBL  PIC  9(002).
               10 BVVEW41I-S-IREDZD-CRDRA-ESTBL PIC  X(030).
               10 BVVEW41I-S-NPRFIL-CRDRA-PGTO  PIC  9(001).
               10 BVVEW41I-S-NSEQ-CONTR-NEGOC   PIC  9(010).
               10 BVVEW41I-S-CBCO               PIC  9(003).
               10 BVVEW41I-S-CAG-BCRIA-CLI      PIC  9(005).
               10 BVVEW41I-S-NCTA-BCRIA         PIC  9(012).
               10 BVVEW41I-S-CTPO-CONTR-CTA     PIC  9(003).
               10 BVVEW41I-S-HINCL-REG          PIC  X(026).
               10 BVVEW41I-S-CUSUAR-INCL        PIC  X(009).
               10 BVVEW41I-S-HMANUT-REG         PIC  X(026).
               10 BVVEW41I-S-CUSUAR-MANUT       PIC  X(007).
               10 BVVEW41I-S-FASE               PIC  9(001).

      ******************************************************************
      *****                    FIM DO BOOK BVVEW41I                 ****
      ******************************************************************
