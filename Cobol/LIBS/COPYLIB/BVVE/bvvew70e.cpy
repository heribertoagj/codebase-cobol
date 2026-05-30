      ******************************************************************
      * NOME BOOK : BVVEW70E - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE ENTRADA                         *
      * DESCRICAO : MANUTENCAO (INCLUSAO)                              *
      *             DE CREDENCIADORAS E BANDEIRAS (OPTIN/OPTOUT)       *
      * DATA      : JANEIRO/2022                                       *
      * AUTOR     : MARCELO / MARIANI                                  *
      * TAMANHO   : 14058 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW70E-CCPF                 : CPF/CNPJ                       *
      * BVVEW70E-CFLIAL-CNPJ          : FILIAL CNPJ                    *
      * BVVEW70E-CCTRL-CPF            : CONTROLE CPF/CNPJ              *
      * BVVEW70E-CBCO                 : BANCO                          *
      * BVVEW70E-CAG-BCRIA            : AGENCIA                        *
      * BVVEW70E-CCTA-BCRIA-CLI       : CONTA                          *
      * BVVEW70E-CTPO-CANAL-CADTO     : CODIGO DO CANAL                *
      * BVVEW70E-CINDCD-CADTO-TODO    : INDICADOR CONTRATACAO REALIZADA*
      *                                 PARA AS CREDENCIADORAS ATIVAS  *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW70E-CINDCD-DOMCL-BCRIO   : DOMICILIADO BANCARIO           *
      *                                 S - SIM                        *
      *                                 N - NAO                        *
      * BVVEW70E-QTD-REG              : TOTAL DE REGISTROS             *
      * BVVEW70E-CCRDRA-ESTBL-COML    : CODIGO CREDENCIADORA           *
      * BVVEW70E-CBANDE-VISAO         : CODIGO BANDEIRA                *
      ******************************************************************
       05 BVVEW70E-HEADER.
          10 BVVEW70E-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW70E'.
          10 BVVEW70E-TAM-LAYOUT           PIC  9(05) VALUE  14058.
       05 BVVEW70E-REGISTRO.
          10 BVVEW70-PARTE-FIXA.
             15 BVVEW70E-CCPF              PIC  9(09).
             15 BVVEW70E-CFLIAL-CNPJ       PIC  9(04).
             15 BVVEW70E-CCTRL-CPF         PIC  9(02).
             15 BVVEW70E-CBCO              PIC  9(03).
             15 BVVEW70E-CAG-BCRIA         PIC  9(05).
             15 BVVEW70E-CCTA-BCRIA-CLI    PIC  9(13).
             15 BVVEW70E-CTPO-CANAL-CADTO  PIC  9(03).
             15 BVVEW70E-CINDCD-CADTO-TODO PIC  X(01).
             15 BVVEW70E-CINDCD-DOMCL-BCRIO
                                           PIC  X(01).
          10 BVVEW70-PARTE-VARIAVEL.
             15 BVVEW70E-QTD-REG           PIC  9(04).
             15 BVVEW70E-TABELA OCCURS  0  TO   2000  TIMES
                DEPENDING ON BVVEW70E-QTD-REG.
                20 BVVEW70E-CCRDRA-ESTBL-COML
                                           PIC  9(04).
                20 BVVEW70E-CBANDE-VISAO   PIC  9(03).
