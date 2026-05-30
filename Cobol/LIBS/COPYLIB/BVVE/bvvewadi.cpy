      ******************************************************************
      * NOME BOOK : BVVEWADI - INTERFACE COORDENADOR/FUNCIONAL         *
      *                        BOOK DE ENTRADA/SAIDA                   *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTOUT                                        *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 3037 BYTES                                         *
      * ---------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      * ---------------------------------------------------------------*
      * BVVEWADI-E-CCPF              : CPF/CNPJ                        *
      * BVVEWADI-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWADI-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWADI-S-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWADI-S-IREDZD-CRDRA-ESTBL: DESCRICAO CREDENCIADORA         *
      * BVVEWADI-S-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWADI-S-RBANDE-VISAO      : DESCRICAO BANDEIRA              *
      * BVVEWADI-S-CINDCD-DOMCL-BCRIO: DOMICILIADO BANCARIO            *
      *                                S - SIM                         *
      *                                N - NAO                         *
      * BVVEWADI-S-CSIT-CADTO-SOLTC  : SITUACAO SOLICITACAO OPT-IN/OUT *
      *                                0 - AGUARDANDO PROCESSAMENTO    *
      *                                1 - ENVIADA PARA CIP            *
      *                                2 - PROCESSADO COM SUCESSO      *
      *                                3 - PROCESSADO COM ERRO         *
      ******************************************************************
      *                - - - A L T E R A C O E S - - -                 *
      *----------------------------------------------------------------*
      * DATA..........: FEVEREIRO / 2022          REMARKS....: VICT    *
      * RESPONSAVEL...: VICTORINO                                      *
      * OBJETIVO......: INCLUSAO DOS CAMPOS BANCO / AGENCIA E CONTA    *
      *               BVVEWADI-E-CBCO              : BANCO             *
      *               BVVEWADI-E-CAG-BCRIA         : AGENCIA BANCARIA  *
      *               BVVEWADI-E-CCTA-BCRIA-CLI    : CONTA CORRENTE    *
      ******************************************************************
           05 BVVEWADI-HEADER.
              10 BVVEWADI-COD-LAYOUT    PIC  X(08)    VALUE 'BVVEWADI'.
VICT          10 BVVEWADI-TAM-LAYOUT    PIC  9(05)    VALUE 3037.
           05 BVVEWADI-REGISTRO.
              10 BVVEWAAE-BLOCO-ENTRADA.
                 15 BVVEWADI-E-CCPF                   PIC  9(009).
                 15 BVVEWADI-E-CFLIAL-CNPJ            PIC  9(004).
                 15 BVVEWADI-E-CCTRL-CPF              PIC  9(002).
VICT             15 BVVEWADI-E-CBCO                   PIC  9(003).
VICT             15 BVVEWADI-E-CAG-BCRIA              PIC  9(005).
VICT             15 BVVEWADI-E-CCTA-BCRIA-CLI         PIC  9(013).
              10 BVVEWADI-BLOCO-PAGINACAO.
                 15 BVVEWADI-INDICADOR-PAGINACAO      PIC  X(001).
                    88 BVVEWADI-P-INICIAL             VALUE 'I'.
                    88 BVVEWADI-P-PRIMEIRA            VALUE 'P'.
                    88 BVVEWADI-P-SEGUINTE            VALUE 'S'.
                    88 BVVEWADI-P-ANTERIOR            VALUE 'A'.
                    88 BVVEWADI-P-ULTIMA              VALUE 'U'.
                 15 BVVEWADI-PRI-CHAVE.
                    20 BVVEWADI-PRI-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWADI-PRI-CBANDE-VISAO      PIC  9(003).
                 15 BVVEWADI-ULT-CHAVE.
                    20 BVVEWADI-ULT-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWADI-ULT-CBANDE-VISAO      PIC  9(003).
              10 BVVEWADI-BLOCO-SAIDA.
                 15 BVVEWADI-S-QTD-REG                PIC  9(003).
                 15 BVVEWADI-AREA-SAIDA.
                    20 BVVEWADI-S-TABELA  OCCURS 1 TO 30 TIMES
                                       DEPENDING ON BVVEWADI-S-QTD-REG.
                       25 BVVEWADI-S-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWADI-S-IREDZD-CRDRA-ESTBL    PIC  X(030).
                       25 BVVEWADI-S-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWADI-S-RBANDE-VISAO          PIC  X(060).
                       25 BVVEWADI-S-CINDCD-DOMCL-BCRIO    PIC  X(001).
                       25 BVVEWADI-S-CSIT-CADTO-SOLTC      PIC  9(001).
