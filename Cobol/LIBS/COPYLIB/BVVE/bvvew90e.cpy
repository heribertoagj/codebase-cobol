      ******************************************************************
      * NOME BOOK : BVVEW90E - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE ENTRADA                         *
      * DESCRICAO : MANUTENCAO (EXCLUSAO)                              *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTOUT)             *
      * DATA      : SETEMBRO/2022                                      *
      * AUTOR     : MARCELO                                            *
      * TAMANHO   : 14054 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW90E-CCPF                 : CPF/CNPJ                       *
      * BVVEW90E-CFLIAL-CNPJ          : FILIAL CNPJ                    *
      * BVVEW90E-CCTRL-CPF            : CONTROLE CPF/CNPJ              *
      * BVVEW90E-CBCO                 : BANCO                          *
      * BVVEW90E-CAG-BCRIA            : AGENCIA                        *
      * BVVEW90E-CCTA-BCRIA-CLI       : CONTA                          *
      * BVVEW90E-DIG-CCTA-BCRIA       : DIGITO DA CONTA                *
      * BVVEW90E-CTPO-CANAL-CADTO     : CODIGO DO CANAL                *
      * BVVEW90E-CINDCD-CADTO-TODO    : INDICADOR CONTRATACAO REALIZADA*
      *                                 PARA AS CREDENCIADORAS ATIVAS  *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW90E-CINDCD-DOMCL-BCRIO   : DOMICILIADO BANCARIO           *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW90E-TIPO-MANUTENCAO      : TIPO DE MANUTENCAO
      *                                 I - INCLUSAO                   *
      *                                 E - EXCLUSAO                   *
      * BVVEW90E-QTD-REG              : TOTAL DE REGISTROS             *
      * BVVEW90E-CCRDRA-ESTBL-COML    : CODIGO CREDENCIADORA           *
      * BVVEW90E-CBANDE-VISAO         : CODIGO BANDEIRA                *
      ******************************************************************
       05 BVVEW90E-HEADER.
          10 BVVEW90E-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW90E'.
          10 BVVEW90E-TAM-LAYOUT           PIC  9(05) VALUE  14054.
       05 BVVEW90E-REGISTRO.
          10 BVVEW90E-PARTE-FIXA.
             15 BVVEW90E-CCPF              PIC  9(09).
             15 BVVEW90E-CFLIAL-CNPJ       PIC  9(04).
             15 BVVEW90E-CCTRL-CPF         PIC  9(02).
             15 BVVEW90E-CBCO              PIC  9(03).
             15 BVVEW90E-CAG-BCRIA         PIC  9(05).
             15 BVVEW90E-CCTA-BCRIA-CLI    PIC  9(07).
             15 BVVEW90E-DIG-CCTA-BCRIA    PIC  9(01).
             15 BVVEW90E-CTPO-CANAL-CADTO  PIC  9(03).
             15 BVVEW90E-CINDCD-CADTO-TODO PIC  X(01).
             15 BVVEW90E-CINDCD-DOMCL-BCRIO
                                           PIC  X(01).
             15 BVVEW90E-TIPO-MANUTENCAO   PIC  X(01).

          10 BVVEW90E-PARTE-VARIAVEL.
             15 BVVEW90E-QTD-REG           PIC  9(04).
             15 BVVEW90E-TABELA OCCURS  0  TO   2000  TIMES
                DEPENDING ON BVVEW90E-QTD-REG.
                20 BVVEW90E-CCRDRA-ESTBL-COML
                                           PIC  9(04).
                20 BVVEW90E-CBANDE-VISAO   PIC  9(03).
