      ******************************************************************
      * NOME BOOK : BVVEWAAW - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE PAGINACAO                       *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 00030 BYTES                                        *
      ******************************************************************
           05 BVVEWAAW-HEADER.
               10 BVVEWAAW-COD-LAYOUT  PIC  X(08)     VALUE 'BVVEWAAW'.
               10 BVVEWAAW-TAM-LAYOUT  PIC  9(05)     VALUE 30.
           05 BVVEWAAW-REGISTRO.
              10 BVVEWAAW-BLOCO-PAGINACAO.
                 15 BVVEWAAW-P-COD-RETORNO            PIC  9(002).
                 15 BVVEWAAW-INDICADOR-PAGINACAO      PIC  X(001).
                    88 BVVEWAAW-P-INICIAL             VALUE 'I'.
                    88 BVVEWAAW-P-PRIMEIRA            VALUE 'P'.
                    88 BVVEWAAW-P-SEGUINTE            VALUE 'S'.
                    88 BVVEWAAW-P-ANTERIOR            VALUE 'A'.
                    88 BVVEWAAW-P-ULTIMA              VALUE 'U'.
                 15 BVVEWAAW-PRI-CHAVE.
                    20 BVVEWAAW-PRI-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWAAW-PRI-CBANDE-VISAO      PIC  9(003).
                 15 BVVEWAAW-ULT-CHAVE.
                    20 BVVEWAAW-ULT-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWAAW-ULT-CBANDE-VISAO      PIC  9(003).
