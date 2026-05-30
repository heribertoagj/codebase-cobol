      ******************************************************************
      * NOME BOOK : BVVEW60E - BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : DEZEMBRO/2021                                      *
      * AUTOR     : MARCELO DIAS - SISCONSULTORIA                      *
      * TAMANHO   : 00053 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW60E-E-CCPF              : CPF/CNPJ                        *
      * BVVEW60E-E-CFLIAL-CNPJ       : FILIAL CNPJ                     *
      * BVVEW60E-E-CCTRL-CPF         : CONTROLE CPF/CNPJ               *
      * BVVEW60E-E-CBCO              : CODIGO DO BANCO                 *
      * BVVEW60E-E-CAG-BCRIA         : CODIGO DE AGENCIA CLIENTE       *
      * BVVEW60E-E-CCTA-BCRIA-CLI    : NUMERO DA CONTA CLIENTE         *
      * BVVEW60E-E-CCRDRA-ESTBL-COML : CODIGO DA CREDENCIADORA         *
      ******************************************************************
       05 BVVEW60E-HEADER.
          10 BVVEW60E-COD-LAYOUT            PIC  X(08) VALUE 'BVVEW60E'.
          10 BVVEW60E-TAM-LAYOUT            PIC  9(05) VALUE  53.
       05 BVVEW60E-E-REGISTRO.
          10 BVVEW60E-E-BLOCO-ENTRADA.
             15 BVVEW60E-E-CCPF             PIC  9(09).
             15 BVVEW60E-E-CFLIAL-CNPJ      PIC  9(04).
             15 BVVEW60E-E-CCTRL-CPF        PIC  9(02).
             15 BVVEW60E-E-CBCO             PIC  9(03).
             15 BVVEW60E-E-CAG-BCRIA        PIC  9(05).
             15 BVVEW60E-E-CCTA-BCRIA-CLI   PIC  9(13).
             15 BVVEW60E-E-CCRDRA-ESTBL-COML
                                            PIC  9(04).
