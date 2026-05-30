      ******************************************************************
      * NOME BOOK : BVVEW044                                           *
      * DESCRICAO : TABELA  DB2PRD.TBANDE_VISAO_EXTER                  *
      * DATA      : AGOSTO/2019                                        *
      * AUTOR     : DESIREE PESSOA                                     *
      * EMPRESA   : BRADESCO - RTB                                     *
      * COMPONENTE: BVVE                                               *
      * TAMANHO   : 256 BYTES                                          *
      **---------------------------------------------------------------*
      * DADOS DO COPYBOOK                                              *
      *----------------------------------------------------------------*
      *   BVVEW044-E-C-ATIVIDADE = INDICATICO DE 'I' = INCLUSAO        *
      *                                          'C' = CONSULTA        *
      *                                          'A' = ALTERACAO       *
      *                                          'D' = DELECAO         *
      *   BVVEW044-E-C-FUNCAO   = INDICATIVO DE FUNCAO DO ACESSO       *
      *        01  - RECUPERA TODAS AS INFORMACOES DA TABELA           *
      *                                                                *
      ******************************************************************
      * DATA        AUTOR            DESCRICAO / MANUTENCAO            *
      * ----------  ---------------  -------------------------------   *
      *                                                                *
      ******************************************************************
         05 BVVEW044-HEADER.
            10 BVVEW044-COD-LAYOUT          PIC X(008) VALUE 'BVVEW044'.
            10 BVVEW044-TAM-LAYOUT          PIC 9(005) VALUE 256.

         05 BVVEW044-CONTROLE.
            10 BVVEW044-E-C-ATIVIDADE        PIC  X(001).
            10 BVVEW044-E-C-FUNCAO           PIC  X(003).
            10 BVVEW044-S-C-MENSAGEM         PIC  X(079).

         05 BVVEW044-ENTRADA.
            10 BVVEW044-E-CTPO-VISAO-BANDE   PIC  9(003).
            10 BVVEW044-E-CBANDE-VISAO       PIC  9(003).
            10 BVVEW044-E-CTPO-AMBTE-EXTER   PIC  9(001).
            10 BVVEW044-E-CBANDE-VISAO-EXTER PIC  X(005).
            10 BVVEW044-E-CUSUAR-INCL        PIC  X(009).
            10 BVVEW044-E-HINCL-REG          PIC  X(026).
            10 BVVEW044-E-CUSUAR-MANUT       PIC  X(007).
            10 BVVEW044-E-HMANUT-REG         PIC  X(026).

         05 BVVEW044-SAIDA.
            10 BVVEW044-S-CTPO-VISAO-BANDE   PIC  9(003).
            10 BVVEW044-S-CBANDE-VISAO       PIC  9(003).
            10 BVVEW044-S-CTPO-AMBTE-EXTER   PIC  9(001).
            10 BVVEW044-S-CBANDE-VISAO-EXTER PIC  X(005).
            10 BVVEW044-S-CUSUAR-INCL        PIC  X(009).
            10 BVVEW044-S-HINCL-REG          PIC  X(026).
            10 BVVEW044-S-CUSUAR-MANUT       PIC  X(007).
            10 BVVEW044-S-HMANUT-REG         PIC  X(026).

      ******************************************************************
      *****                    FIM DO BOOK BVVEW044                 ****
      ******************************************************************
