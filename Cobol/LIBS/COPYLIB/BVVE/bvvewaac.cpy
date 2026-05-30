      ******************************************************************
      * NOME BOOK : BVVEWAAC - INTERFACE FUNCIONAL/ACESSO A DADOS      *
      *                        BOOK DE ENTRADA/SAIDA                   *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 2956 BYTES                                         *
      * ---------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      * ---------------------------------------------------------------*
      * BVVEWAAC-E-CCPF              : CPF/CNPJ                        *
      * BVVEWAAC-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWAAC-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWAAC-S-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWAAC-S-IREDZD-CRDRA-ESTBL: DESCRICAO CREDENCIADORA         *
      * BVVEWAAC-S-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWAAC-S-RBANDE-VISAO      : DESCRICAO BANDEIRA              *
      ******************************************************************
           05 BVVEWAAC-HEADER.
              10 BVVEWAAC-COD-LAYOUT    PIC  X(08)    VALUE 'BVVEWAAC'.
              10 BVVEWAAC-TAM-LAYOUT    PIC  9(05)    VALUE 2956.
           05 BVVEWAAC-REGISTRO.
              10 BVVEWAAE-BLOCO-ENTRADA.
                 15 BVVEWAAC-E-CCPF                   PIC  9(009).
                 15 BVVEWAAC-E-CFLIAL-CNPJ            PIC  9(004).
                 15 BVVEWAAC-E-CCTRL-CPF              PIC  9(002).
              10 BVVEWAAC-BLOCO-PAGINACAO.
                 15 BVVEWAAC-INDICADOR-PAGINACAO      PIC  X(001).
                    88 BVVEWAAC-P-INICIAL             VALUE 'I'.
                    88 BVVEWAAC-P-PRIMEIRA            VALUE 'P'.
                    88 BVVEWAAC-P-SEGUINTE            VALUE 'S'.
                    88 BVVEWAAC-P-ANTERIOR            VALUE 'A'.
                    88 BVVEWAAC-P-ULTIMA              VALUE 'U'.
                 15 BVVEWAAC-PRI-CHAVE.
                    20 BVVEWAAC-PRI-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWAAC-PRI-CBANDE-VISAO      PIC  9(003).
                 15 BVVEWAAC-ULT-CHAVE.
                    20 BVVEWAAC-ULT-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWAAC-ULT-CBANDE-VISAO      PIC  9(003).
              10 BVVEWAAC-BLOCO-SAIDA.
                 15 BVVEWAAC-S-QTD-REG                PIC  9(003).
                 15 BVVEWAAC-AREA-SAIDA.
                    20 BVVEWAAC-S-TABELA  OCCURS 1 TO 30 TIMES
                                       DEPENDING ON BVVEWAAC-S-QTD-REG.
                       25 BVVEWAAC-S-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWAAC-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                       25 BVVEWAAC-S-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWAAC-S-RBANDE-VISAO          PIC  X(060).
