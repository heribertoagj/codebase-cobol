      ******************************************************************
      * NOME BOOK : BVVEWADW - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE PAGINACAO                       *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPT-OUT                                       *
      * DATA      : OUTUBRO/2019                                       *
      * AUTOR     : THAIS TONIOLO - PRIME INFORMATICA                  *
      * TAMANHO   : 00030 BYTES                                        *
      ******************************************************************
           05 BVVEWADW-HEADER.
               10 BVVEWADW-COD-LAYOUT  PIC  X(08)     VALUE 'BVVEWADW'.
               10 BVVEWADW-TAM-LAYOUT  PIC  9(05)     VALUE 30.
           05 BVVEWADW-REGISTRO.
              10 BVVEWADW-BLOCO-PAGINACAO.
                 15 BVVEWADW-P-COD-RETORNO            PIC  9(002).
                 15 BVVEWADW-INDICADOR-PAGINACAO      PIC  X(001).
                    88 BVVEWADW-P-INICIAL             VALUE 'I'.
                    88 BVVEWADW-P-PRIMEIRA            VALUE 'P'.
                    88 BVVEWADW-P-SEGUINTE            VALUE 'S'.
                    88 BVVEWADW-P-ANTERIOR            VALUE 'A'.
                    88 BVVEWADW-P-ULTIMA              VALUE 'U'.
                 15 BVVEWADW-PRI-CHAVE.
                    20 BVVEWADW-PRI-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWADW-PRI-CBANDE-VISAO      PIC  9(003).
                 15 BVVEWADW-ULT-CHAVE.
                    20 BVVEWADW-ULT-CCRDRA-ESTBL-COML PIC  9(004).
                    20 BVVEWADW-ULT-CBANDE-VISAO      PIC  9(003).
