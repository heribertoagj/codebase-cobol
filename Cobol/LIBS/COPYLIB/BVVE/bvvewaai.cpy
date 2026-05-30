      ******************************************************************
      * NOME BOOK : BVVEWAAI - INTERFACE COORDENADOR/FUNCIONAL         *
      *                        BOOK DE ENTRADA/SAIDA                   *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 2956 BYTES                                         *
      * ---------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      * ---------------------------------------------------------------*
      * BVVEWAAI-E-CCPF              : CPF/CNPJ                        *
      * BVVEWAAI-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWAAI-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWAAI-S-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWAAI-S-IREDZD-CRDRA-ESTBL: DESCRICAO CREDENCIADORA         *
      * BVVEWAAI-S-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWAAI-S-RBANDE-VISAO      : DESCRICAO BANDEIRA              *
      ******************************************************************
           05 BVVEWAAI-HEADER.
              10 BVVEWAAI-COD-LAYOUT    PIC  X(08)    VALUE 'BVVEWAAI'.
              10 BVVEWAAI-TAM-LAYOUT    PIC  9(05)    VALUE 2956.
           05 BVVEWAAI-REGISTRO.
              10 BVVEWAAE-BLOCO-ENTRADA.
                 15 BVVEWAAI-E-CCPF                   PIC  9(009).
                 15 BVVEWAAI-E-CFLIAL-CNPJ            PIC  9(004).
                 15 BVVEWAAI-E-CCTRL-CPF              PIC  9(002).
              10 BVVEWAAI-BLOCO-PAGINACAO.
                 15 BVVEWAAI-INDICADOR-PAGINACAO      PIC  X(001).
                    88 BVVEWAAI-P-INICIAL             VALUE 'I'.
                    88 BVVEWAAI-P-PRIMEIRA            VALUE 'P'.
                    88 BVVEWAAI-P-SEGUINTE            VALUE 'S'.
                    88 BVVEWAAI-P-ANTERIOR            VALUE 'A'.
                    88 BVVEWAAI-P-ULTIMA              VALUE 'U'.
                 15 BVVEWAAI-PRI-CHAVE.
                    20 BVVEWAAI-PRI-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWAAI-PRI-CBANDE-VISAO      PIC  9(003).
                 15 BVVEWAAI-ULT-CHAVE.
                    20 BVVEWAAI-ULT-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWAAI-ULT-CBANDE-VISAO      PIC  9(003).
              10 BVVEWAAI-BLOCO-SAIDA.
                 15 BVVEWAAI-S-QTD-REG                PIC  9(003).
                 15 BVVEWAAI-AREA-SAIDA.
                    20 BVVEWAAI-S-TABELA  OCCURS 1 TO 30 TIMES
                                       DEPENDING ON BVVEWAAI-S-QTD-REG.
                       25 BVVEWAAI-S-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWAAI-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                       25 BVVEWAAI-S-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWAAI-S-RBANDE-VISAO          PIC  X(060).
