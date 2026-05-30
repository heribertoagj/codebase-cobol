      ******************************************************************
      * NOME BOOK : BVVEWAAS - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 2926 BYTES                                         *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWAAS-S-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWAAS-S-IREDZD-CRDRA-ESTBL: DESCRICAO CREDENCIADORA         *
      * BVVEWAAS-S-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWAAS-S-RBANDE-VISAO      : DESCRICAO BANDEIRA              *
      ******************************************************************
           05 BVVEWAAS-HEADER.
              10 BVVEWAAS-COD-LAYOUT    PIC  X(08)   VALUE 'BVVEWAAS'.
              10 BVVEWAAS-TAM-LAYOUT    PIC  9(05)   VALUE 2926.
           05 BVVEWAAS-REGISTRO.
              10 BVVEWAAS-BLOCO-SAIDA.
                 15 BVVEWAAS-S-QTD-REG               PIC 9(003).
                 15 BVVEWAAS-AREA-SAIDA.
                    20 BVVEWAAS-S-TABELA  OCCURS 1 TO 30 TIMES
                                       DEPENDING ON BVVEWAAS-S-QTD-REG.
                       25 BVVEWAAS-S-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWAAS-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                       25 BVVEWAAS-S-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWAAS-S-RBANDE-VISAO          PIC  X(060).
