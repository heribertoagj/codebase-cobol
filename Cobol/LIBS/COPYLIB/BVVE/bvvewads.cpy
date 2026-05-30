      ******************************************************************
      * NOME BOOK : BVVEWADS - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTOUT                                        *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 2986 BYTES                                         *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWADS-S-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWADS-S-IREDZD-CRDRA-ESTBL: DESCRICAO CREDENCIADORA         *
      * BVVEWADS-S-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWADS-S-RBANDE-VISAO      : DESCRICAO BANDEIRA              *
      * BVVEWADS-S-CINDCD-DOMCL-BCRIO: DOMICILIADO BANCARIO            *
      *                                S - SIM                         *
      *                                N - NAO                         *
      * BVVEWADS-S-CSIT-CADTO-SOLTC  : SITUACAO SOLICITACAO OPT-IN/OUT *
      *                                0 - AGUARDANDO PROCESSAMENTO    *
      *                                1 - ENVIADA PARA CIP            *
      *                                2 - PROCESSADO COM SUCESSO      *
      *                                3 - PROCESSADO COM ERRO         *
      ******************************************************************
           05 BVVEWADS-HEADER.
              10 BVVEWADS-COD-LAYOUT    PIC  X(08)   VALUE 'BVVEWADS'.
              10 BVVEWADS-TAM-LAYOUT    PIC  9(05)   VALUE 2986.
           05 BVVEWADS-REGISTRO.
              10 BVVEWADS-BLOCO-SAIDA.
                 15 BVVEWADS-S-QTD-REG               PIC 9(003).
                 15 BVVEWADS-AREA-SAIDA.
                    20 BVVEWADS-S-TABELA  OCCURS 1 TO 30 TIMES
                                       DEPENDING ON BVVEWADS-S-QTD-REG.
                       25 BVVEWADS-S-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWADS-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                       25 BVVEWADS-S-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWADS-S-RBANDE-VISAO          PIC  X(060).
                       25 BVVEWADS-S-CINDCD-DOMCL-BCRIO    PIC  X(001).
                       25 BVVEWADS-S-CSIT-CADTO-SOLTC      PIC  9(001).
