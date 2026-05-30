      ******************************************************************
      * NOME BOOK : BVVEW38I                                           *
      * DESCRICAO : TABELA  TARRJO_PGTO                                *
      * DATA      : 20/08/2018                                         *
      * AUTOR     : MARIA GORETI STOPASOLLA PINTO                      *
      * EMPRESA   : BRADESCO - CD                                      *
      * COMPONENTE: BVVE                                               *
      * TAMANHO   : 221 BYTES                                          *
      *----------------------------------------------------------------*
      * DADOS DO COPYBOOK                                              *
      *----------------------------------------------------------------*
      *   BVVEW38I-E-C-ATIVIDADE = INDICATICO DE 'I' = INCLUSAO        *
      *                                          'C' = CONSULTA        *
      *                                          'A' = ALTERACAO       *
      *                                          'D' = DELECAO         *
      *   BVVEW38I-E-C-FUNCAO   = INDICATIVO DE FUNCAO DO ACESSO       *
      *        01  - RECUPERA TODAS AS INFORMACOES DA TABELA           *
      *                                                                *
      ******************************************************************
      * DATA        AUTOR            DESCRICAO / MANUTENCAO            *
      * ----------  ---------------  -------------------------------   *
      *                                                                *
      ******************************************************************
         05 BVVEW38I-HEADER.
            10 BVVEW38I-COD-LAYOUT          PIC X(008) VALUE 'BVVEW38I'.
            10 BVVEW38I-TAM-LAYOUT          PIC 9(005) VALUE 221.

         05 BVVEW38I-CONTROLE.
            10 BVVEW38I-E-C-ATIVIDADE        PIC  X(001).
            10 BVVEW38I-E-C-FUNCAO           PIC  X(003).
            10 BVVEW38I-S-MENSAGEM           PIC  X(079).

         05 BVVEW38I-ENTRADA.
            10 BVVEW38I-E-CARRJO-PGTO        PIC  9(003).
            10 BVVEW38I-E-IARRJO-PGTO        PIC  X(030).
            10 BVVEW38I-E-CTPO-ARRJO-PGTO    PIC  X(001).
            10 BVVEW38I-E-HINCL-REG          PIC  X(026).
            10 BVVEW38I-E-CUSUAR-INCL        PIC  X(009).
            10 BVVEW38I-E-HMANUT-REG         PIC  X(026).
            10 BVVEW38I-E-CUSUAR-MANUT       PIC  X(007).

         05 BVVEW38I-SAIDA.
            10 BVVEW38I-S-CARRJO-PGTO        PIC  9(003).
            10 BVVEW38I-S-IARRJO-PGTO        PIC  X(030).
            10 BVVEW38I-S-CTPO-ARRJO-PGTO    PIC  X(001).
            10 BVVEW38I-S-HINCL-REG          PIC  X(026).
            10 BVVEW38I-S-CUSUAR-INCL        PIC  X(009).
            10 BVVEW38I-S-HMANUT-REG         PIC  X(026).
            10 BVVEW38I-S-CUSUAR-MANUT       PIC  X(007).

      ******************************************************************
      *****                    FIM DO BOOK BVVEW38I                 ****
      ******************************************************************
