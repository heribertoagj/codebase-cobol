      ******************************************************************
      * NOME BOOK : BVVEWABC - INTERFACE FUNCIONAL/ACESSO A DADOS      *
      *                        BOOK DE ENTRADA/SAIDA                   *
      * DESCRICAO : MANUTENCAO (INCLUSAO/ALTERACAO/EXCLUSAO)           *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTIN/OPTOUT)       *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME                              *
      * TAMANHO   : 18064 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWABC-E-CUSUAR            : USUARIO                         *
      * BVVEWABC-E-CCPF              : CPF/CNPJ                        *
      * BVVEWABC-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWABC-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWABC-E-CBCO              : BANCO                           *
      * BVVEWABC-E-CAG-BCRIA         : AGENCIA                         *
      * BVVEWABC-E-CCTA-BCRIA-CLI    : CONTA                           *
      * BVVEWABC-E-DIG-CCTA-BCRIA    : DIGITO CONTA                    *
      * BVVEWABC-E-CINDCD-CADTO-TODO : INDICADOR DE CADASTRO REALIZADO *
      *                                PARA TODOS OS PRODUTOS ATIVOS   *
      *                                S - SIM                         *
      *                                N - NAO                         *
      * BVVEWABC-E-QTD-REG           : QUANTIDADE DE CREDENCIADORAS E  *
      *                                BANDEIRAS PARA MANUTENCAO       *
      * BVVEWABC-E-TIPO-MANUTENCAO   : TIPO DE MANUTENCAO              *
      *                                I - INCLUSAO                    *
      *                                A - ALTERACAO                   *
      *                                E - EXCLUSAO                    *
      * BVVEWABC-E-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWABC-E-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWABC-E-CINDCD-DOMCL-BCRIO: DOMICILIADO BANCARIO            *
      *                                S - SIM                         *
      *                                N - NAO                         *
      ******************************************************************
           05 BVVEWABC-HEADER.
              10 BVVEWABC-COD-LAYOUT    PIC  X(08)    VALUE 'BVVEWABC'.
              10 BVVEWABC-TAM-LAYOUT    PIC  9(05)    VALUE 18064.
           05 BVVEWABC-REGISTRO.
              10 BVVEWABC-BLOCO-ENTRADA.
                 15 BVVEWABC-E-CUSUAR                 PIC X(009).
                 15 BVVEWABC-E-CCPF                   PIC 9(009).
                 15 BVVEWABC-E-CFLIAL-CNPJ            PIC 9(004).
                 15 BVVEWABC-E-CCTRL-CPF              PIC 9(002).
                 15 BVVEWABC-E-CBCO                   PIC 9(003).
                 15 BVVEWABC-E-CAG-BCRIA              PIC 9(005).
                 15 BVVEWABC-E-CCTA-BCRIA-CLI         PIC 9(013).
                 15 BVVEWABC-E-DIG-CCTA-BCRIA         PIC X(001).
                 15 BVVEWABC-E-CINDCD-CADTO-TODO      PIC X(001).
                 15 BVVEWABC-E-QTD-REG                PIC 9(004).
                 15 BVVEWABC-AREA-ENTRADA.
                    20 BVVEWABC-E-TABELA  OCCURS 1 TO 2000 TIMES
                                       DEPENDING ON BVVEWABC-E-QTD-REG.
                       25 BVVEWABC-E-TIPO-MANUTENCAO       PIC  X(001).
                       25 BVVEWABC-E-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWABC-E-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWABC-E-CINDCD-DOMCL-BCRIO    PIC  X(001).
