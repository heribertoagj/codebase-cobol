      ******************************************************************
      * NOME BOOK : BVVEWABI - INTERFACE COORDENADOR/FUNCIONAL         *
      *                        BOOK DE ENTRADA/SAIDA                   *
      * DESCRICAO : MANUTENCAO (INCLUSAO/ALTERACAO/EXCLUSAO)           *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTIN/OPTOUT)       *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME                              *
      * TAMANHO   : 18064 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWABI-E-CUSUAR            : USUARIO                         *
      * BVVEWABI-E-CCPF              : CPF/CNPJ                        *
      * BVVEWABI-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWABI-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWABI-E-CBCO              : BANCO                           *
      * BVVEWABI-E-CAG-BCRIA         : AGENCIA                         *
      * BVVEWABI-E-CCTA-BCRIA-CLI    : CONTA                           *
      * BVVEWABI-E-DIG-CCTA-BCRIA    : DIGITO CONTA                    *
      *                                REALIZADO PARA A AGENDA         *
      *                                I - INCLUSAO (OPTIN)            *
      *                                E - EXCLUSAO (OPTOUT)           *
      * BVVEWABI-E-CINDCD-CADTO-TODO : INDICADOR DE CADASTRO REALIZADO *
      *                                PARA TODOS OS PRODUTOS ATIVOS   *
      *                                S - SIM                         *
      *                                N - NAO                         *
      * BVVEWABI-E-QTD-REG           : QUANTIDADE DE CREDENCIADORAS E  *
      *                                BANDEIRAS PARA INCLUSAO         *
      * BVVEWABI-E-TIPO-MANUTENCAO   : TIPO DE MANUTENCAO              *
      *                                I - INCLUSAO                    *
      *                                A - ALTERACAO                   *
      *                                E - EXCLUSAO                    *
      * BVVEWABI-E-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWABI-E-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWABI-E-CINDCD-DOMCL-BCRIO: DOMICILIADO BANCARIO            *
      *                                S - SIM                         *
      *                                N - NAO                         *
      ******************************************************************
           05 BVVEWABI-HEADER.
              10 BVVEWABI-COD-LAYOUT    PIC  X(08)    VALUE 'BVVEWABI'.
              10 BVVEWABI-TAM-LAYOUT    PIC  9(05)    VALUE 18064.
           05 BVVEWABI-REGISTRO.
              10 BVVEWABI-BLOCO-ENTRADA.
                 15 BVVEWABI-E-CUSUAR                 PIC X(009).
                 15 BVVEWABI-E-CCPF                   PIC 9(009).
                 15 BVVEWABI-E-CFLIAL-CNPJ            PIC 9(004).
                 15 BVVEWABI-E-CCTRL-CPF              PIC 9(002).
                 15 BVVEWABI-E-CBCO                   PIC 9(003).
                 15 BVVEWABI-E-CAG-BCRIA              PIC 9(005).
                 15 BVVEWABI-E-CCTA-BCRIA-CLI         PIC 9(013).
                 15 BVVEWABI-E-DIG-CCTA-BCRIA         PIC X(001).
                 15 BVVEWABI-E-CINDCD-CADTO-TODO      PIC X(001).
                 15 BVVEWABI-E-QTD-REG                PIC 9(004).
                 15 BVVEWABI-AREA-ENTRADA.
                    20 BVVEWABI-E-TABELA  OCCURS 1 TO 2000 TIMES
                                       DEPENDING ON BVVEWABI-E-QTD-REG.
                       25 BVVEWABI-E-TIPO-MANUTENCAO       PIC  X(001).
                       25 BVVEWABI-E-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWABI-E-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWABI-E-CINDCD-DOMCL-BCRIO    PIC  X(001).
