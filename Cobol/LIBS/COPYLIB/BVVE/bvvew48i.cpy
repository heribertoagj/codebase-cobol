      ******************************************************************
      * NOME BOOK : BVVEW48I                                           *
      * DESCRICAO : TABELA DB2PRD.TPRODT_AGNDA_FINCR                   *
      * DATA      : AGOSTO/2019                                        *
      * AUTOR     : SIVEK, JOSEMAR                                     *
      * EMPRESA   : BRADESCO - CTB                                     *
      * COMPONENTE: BVVE                                               *
      * TAMANHO   : 306 BYTES                                          *
      **---------------------------------------------------------------*
      * DADOS DO COPYBOOK                                              *
      *----------------------------------------------------------------*
      *   BVVEW48I-E-C-ATIVIDADE = INDICATICO DE 'I' = INCLUSAO        *
      *                                          'C' = CONSULTA        *
      *                                          'A' = ALTERACAO       *
      *   BVVEW48I-E-C-FUNCAO   = INDICATIVO DE FUNCAO DO ACESSO       *
      *        01  - RECUPERA TODAS AS INFORMACOES DA TABELA           *
      *                                                                *
      ******************************************************************
      * DATA        AUTOR            DESCRICAO / MANUTENCAO            *
      * ----------  ---------------  -------------------------------   *
      *                                                                *
      ******************************************************************
         05 BVVEW48I-HEADER.
            10 BVVEW48I-COD-LAYOUT     PIC  X(008) VALUE 'BVVEW48I'.
            10 BVVEW48I-TAM-LAYOUT     PIC  9(005) VALUE 306.

         05 BVVEW48I-CONTROLE.
            10 BVVEW48I-E-C-ATIVIDADE  PIC  X(001).
            10 BVVEW48I-E-C-FUNCAO     PIC  X(003).
            10 BVVEW48I-S-C-MENSAGEM   PIC  X(079).

         05 BVVEW48I-ENTRADA.
            10 BVVEW48I-E-CPRODT-AGNDA-FINCR
                                       PIC  9(005).
            10 BVVEW48I-E-CCRDRA-ESTBL-COML
                                       PIC  9(004).
            10 BVVEW48I-E-CTPO-VISAO-BANDE
                                       PIC  9(003).
            10 BVVEW48I-E-CBANDE-VISAO PIC  9(003).
            10 BVVEW48I-E-CINDCD-ORD-PGTO
                                       PIC  9(001).
            10 BVVEW48I-E-IPRODT-AGNDA-FINCR
                                       PIC  X(020).
            10 BVVEW48I-E-CINDCD-SIT-REG
                                       PIC  X(001).
            10 BVVEW48I-E-CUSUAR-INCL  PIC  X(009).
            10 BVVEW48I-E-HINCL-REG    PIC  X(026).
            10 BVVEW48I-E-CUSUAR-MANUT PIC  X(007).
            10 BVVEW48I-E-HMANUT-REG   PIC  X(026).

         05 BVVEW48I-SAIDA.
            10 BVVEW48I-S-CPRODT-AGNDA-FINCR
                                       PIC  9(005).
            10 BVVEW48I-S-CCRDRA-ESTBL-COML
                                       PIC  9(004).
            10 BVVEW48I-S-CTPO-VISAO-BANDE
                                       PIC  9(003).
            10 BVVEW48I-S-CBANDE-VISAO PIC  9(003).
            10 BVVEW48I-S-CINDCD-ORD-PGTO
                                       PIC  9(001).
            10 BVVEW48I-S-IPRODT-AGNDA-FINCR
                                       PIC  X(020).
            10 BVVEW48I-S-CINDCD-SIT-REG
                                       PIC  X(001).
            10 BVVEW48I-S-CUSUAR-INCL  PIC  X(009).
            10 BVVEW48I-S-HINCL-REG    PIC  X(026).
            10 BVVEW48I-S-CUSUAR-MANUT PIC  X(007).
            10 BVVEW48I-S-HMANUT-REG   PIC  X(026).

      ******************************************************************
      *****                    FIM DO BOOK BVVEW48I                 ****
      ******************************************************************
