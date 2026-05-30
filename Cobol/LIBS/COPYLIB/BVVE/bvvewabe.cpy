      ******************************************************************
      * NOME BOOK : BVVEWABE - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE ENTRADA                         *
      * DESCRICAO : MANUTENCAO (INCLUSAO)                              *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTIN/OPTOUT)       *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME                              *
      * TAMANHO   : 16055 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWABE-E-CCPF              : CPF/CNPJ                        *
      * BVVEWABE-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWABE-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWABE-E-CBCO              : BANCO                           *
      * BVVEWABE-E-CAG-BCRIA         : AGENCIA                         *
      * BVVEWABE-E-CCTA-BCRIA-CLI    : CONTA                           *
      * BVVEWABE-E-DIG-CCTA-BCRIA    : DIGITO CONTA                    *
      * BVVEWABE-E-CINDCD-CADTO-TODO : INDICADOR DE CADASTRO REALIZADO *
      *                                PARA TODOS OS PRODUTOS ATIVOS   *
      *                                S - SIM                         *
      *                                N - NAO                         *
      * BVVEWABE-E-QTD-REG           : QUANTIDADE DE CREDENCIADORAS E  *
      *                                BANDEIRAS PARA INCLUSAO         *
      * BVVEWABE-E-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWABE-E-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWABE-E-CINDCD-DOMCL-BCRIO: DOMICILIADO BANCARIO            *
      *                                S - SIM                         *
      *                                N - NAO                         *
      ******************************************************************
           05 BVVEWABE-HEADER.
              10 BVVEWABE-COD-LAYOUT    PIC  X(08)    VALUE 'BVVEWABE'.
              10 BVVEWABE-TAM-LAYOUT    PIC  9(05)    VALUE 16055.
           05 BVVEWABE-REGISTRO.
              10 BVVEWABE-BLOCO-ENTRADA.
                 15 BVVEWABE-E-CCPF                   PIC  9(009).
                 15 BVVEWABE-E-CFLIAL-CNPJ            PIC  9(004).
                 15 BVVEWABE-E-CCTRL-CPF              PIC  9(002).
                 15 BVVEWABE-E-CBCO                   PIC  9(003).
                 15 BVVEWABE-E-CAG-BCRIA              PIC  9(005).
                 15 BVVEWABE-E-CCTA-BCRIA-CLI         PIC  9(013).
                 15 BVVEWABE-E-DIG-CCTA-BCRIA         PIC  X(001).
                 15 BVVEWABE-E-CINDCD-CADTO-TODO      PIC  X(001).
                 15 BVVEWABE-E-QTD-REG                PIC  9(004).
                 15 BVVEWABE-AREA-ENTRADA.
                    20 BVVEWABE-E-TABELA  OCCURS 1 TO 2000 TIMES
                                       DEPENDING ON BVVEWABE-E-QTD-REG.
                       25 BVVEWABE-E-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWABE-E-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWABE-E-CINDCD-DOMCL-BCRIO    PIC  X(001).
