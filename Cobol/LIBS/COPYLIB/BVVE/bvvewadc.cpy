      ******************************************************************
      * NOME BOOK : BVVEWADC - INTERFACE FUNCIONAL/ACESSO A DADOS      *
      *                        BOOK DE ENTRADA/SAIDA                   *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTOUT                                        *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 3016 BYTES                                         *
      * ---------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      * ---------------------------------------------------------------*
      * BVVEWADC-E-CCPF              : CPF/CNPJ                        *
      * BVVEWADC-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWADC-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWADC-S-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWADC-S-IREDZD-CRDRA-ESTBL: DESCRICAO CREDENCIADORA         *
      * BVVEWADC-S-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWADC-S-RBANDE-VISAO      : DESCRICAO BANDEIRA              *
      * BVVEWADC-S-CINDCD-DOMCL-BCRIO: DOMICILIADO BANCARIO            *
      *                                S - SIM                         *
      *                                N - NAO                         *
      * BVVEWADC-S-CSIT-CADTO-SOLTC  : SITUACAO SOLICITACAO OPT-IN/OUT *
      *                                0 - AGUARDANDO PROCESSAMENTO    *
      *                                1 - ENVIADA PARA CIP            *
      *                                2 - PROCESSADO COM SUCESSO      *
      *                                3 - PROCESSADO COM ERRO-OPTIN   *
      *                                4 - PROCESSADO COM ERRO-OPTOUT  *
      *                                                                *
      ******************************************************************
      *                - - - A L T E R A C O E S - - -                 *
      *----------------------------------------------------------------*
      * DATA..........: FEVEREIRO / 2022          REMARKS....: VICT    *
      * RESPONSAVEL...: VICTORINO                                      *
      * OBJETIVO......: INCLUSAO DOS CAMPOS BANCO / AGENCIA E CONTA    *
      *               BVVEWADC-E-CBCO              : BANCO             *
      *               BVVEWADC-E-CAG-BCRIA         : AGENCIA BANCARIA  *
      *               BVVEWADC-E-CCTA-BCRIA-CLI    : CONTA CORRENTE    *
      ******************************************************************
           05 BVVEWADC-HEADER.
              10 BVVEWADC-COD-LAYOUT    PIC  X(08)    VALUE 'BVVEWADC'.
VICT          10 BVVEWADC-TAM-LAYOUT    PIC  9(05)    VALUE 3037.
           05 BVVEWADC-REGISTRO.
              10 BVVEWAAE-BLOCO-ENTRADA.
                 15 BVVEWADC-E-CCPF                   PIC  9(009).
                 15 BVVEWADC-E-CFLIAL-CNPJ            PIC  9(004).
                 15 BVVEWADC-E-CCTRL-CPF              PIC  9(002).
VICT             15 BVVEWADC-E-CBCO                   PIC  9(003).
VICT             15 BVVEWADC-E-CAG-BCRIA              PIC  9(005).
VICT             15 BVVEWADC-E-CCTA-BCRIA-CLI         PIC  9(013).
              10 BVVEWADC-BLOCO-PAGINACAO.
                 15 BVVEWADC-INDICADOR-PAGINACAO      PIC  X(001).
                    88 BVVEWADC-P-INICIAL             VALUE 'I'.
                    88 BVVEWADC-P-PRIMEIRA            VALUE 'P'.
                    88 BVVEWADC-P-SEGUINTE            VALUE 'S'.
                    88 BVVEWADC-P-ANTERIOR            VALUE 'A'.
                    88 BVVEWADC-P-ULTIMA              VALUE 'U'.
                 15 BVVEWADC-PRI-CHAVE.
                    20 BVVEWADC-PRI-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWADC-PRI-CBANDE-VISAO      PIC  9(003).
                 15 BVVEWADC-ULT-CHAVE.
                    20 BVVEWADC-ULT-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWADC-ULT-CBANDE-VISAO      PIC  9(003).
              10 BVVEWADC-BLOCO-SAIDA.
                 15 BVVEWADC-S-QTD-REG                PIC  9(003).
                 15 BVVEWADC-AREA-SAIDA.
                    20 BVVEWADC-S-TABELA  OCCURS 1 TO 30 TIMES
                                       DEPENDING ON BVVEWADC-S-QTD-REG.
                       25 BVVEWADC-S-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWADC-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                       25 BVVEWADC-S-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWADC-S-RBANDE-VISAO          PIC  X(060).
                       25 BVVEWADC-S-CINDCD-DOMCL-BCRIO    PIC  X(001).
                       25 BVVEWADC-S-CSIT-CADTO-SOLTC      PIC  9(001).
