      ******************************************************************
      * NOME BOOK : BVVEWAEE - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE ENTRADA                         *
      * DESCRICAO : MANUTENCAO (ALTERACAO/EXCLUSAO)                    *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTIN/OPTOUT)       *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 18055 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEWAEE-E-CCPF              : CPF/CNPJ                        *
      * BVVEWAEE-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEWAEE-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEWAEE-E-CBCO              : BANCO                           *
      * BVVEWAEE-E-CAG-BCRIA         : AGENCIA                         *
      * BVVEWAEE-E-CCTA-BCRIA-CLI    : CONTA                           *
      * BVVEWAEE-E-DIG-CCTA-BCRIA    : DIGITO CONTA
      * BVVEWAEE-E-CINDCD-CADTO-TODO : INDICADOR DE CADASTRO REALIZADO *
      *                                PARA TODOS OS PRODUTOS ATIVOS   *
      *                                S - SIM                         *
      *                                N - NAO                         *
      * BVVEWAEE-E-QTD-REG           : QUANTIDADE DE CREDENCIADORAS E  *
      *                                BANDEIRAS PARA EXCLUSAO OPT-OUT *
      * BVVEWAEE-E-TIPO-MANUTENCAO   : TIPO DE MANUTENCAO              *
      *                                I - INCLUSAO                    *
      *                                A - ALTERACAO                   *
      *                                E - EXCLUSAO                    *
      * BVVEWAEE-E-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEWAEE-E-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      * BVVEWAEE-E-CINDCD-DOMCL-BCRIO: DOMICILIADO BANCARIO            *
      *                                S - SIM                         *
      *                                N - NAO                         *
      ******************************************************************
           05 BVVEWAEE-HEADER.
              10 BVVEWAEE-COD-LAYOUT    PIC  X(08)   VALUE 'BVVEWAEE'.
              10 BVVEWAEE-TAM-LAYOUT    PIC  9(05)   VALUE 18055.
           05 BVVEWAEE-REGISTRO.
              10 BVVEWAEE-BLOCO-ENTRADA.
                 15 BVVEWAEE-E-CCPF                  PIC  9(009).
                 15 BVVEWAEE-E-CFLIAL-CNPJ           PIC  9(004).
                 15 BVVEWAEE-E-CCTRL-CPF             PIC  9(002).
                 15 BVVEWAEE-E-CBCO                  PIC  9(003).
                 15 BVVEWAEE-E-CAG-BCRIA             PIC  9(005).
                 15 BVVEWAEE-E-CCTA-BCRIA-CLI        PIC  9(013).
                 15 BVVEWAEE-E-DIG-CCTA-BCRIA        PIC  X(001).
                 15 BVVEWAEE-E-CINDCD-CADTO-TODO     PIC  X(001).
                 15 BVVEWAEE-E-QTD-REG               PIC  9(004).
                 15 BVVEWAEE-AREA-ENTRADA.
                    20 BVVEWAEE-E-TABELA  OCCURS 1 TO 2000 TIMES
                                       DEPENDING ON BVVEWAEE-E-QTD-REG.
                       25 BVVEWAEE-E-TIPO-MANUTENCAO       PIC  X(001).
                       25 BVVEWAEE-E-CCRDRA-ESTBL-COML     PIC  9(004).
                       25 BVVEWAEE-E-CBANDE-VISAO          PIC  9(003).
                       25 BVVEWAEE-E-CINDCD-DOMCL-BCRIO    PIC  X(001).
