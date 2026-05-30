      ******************************************************************
      * NOME BOOK : CRURWWJI                                           *
      * DESCRICAO : BOOK DE INTERFACE COM FUNCIONAL CRUR3WJL           *
      * DATA      : 11/04/2013                                         *
      * AUTOR     : RODRIGO ROCHA                                      *
      * EMPRESA   : CPM BRAXIS                                         *
      * GRUPO     : TI MELHORIAS - CPM BRAXIS                          *
      * TAMANHO   : 15471  BYTES                                       *
      ******************************************************************
       05  CRURWWJI-HEADER.
           10 CRURWWJI-COD-LAYOUT      PIC X(008)      VALUE 'CRURWWJI'.
           10 CRURWWJI-TAM-LAYOUT      PIC 9(005)      VALUE 15471.
       05  CRURWWJI-REGISTRO.
           10 CRURWWJI-BLOCO-ENTRADA.
               15 CRURWWJI-E-MAX-OCORR                 PIC 9(05).
               15 CRURWWJI-E-NVRSAO-GRP-FLUXO          PIC 9(02).
               15 CRURWWJI-E-CGRP-INFO-FLUXO           PIC 9(03).
               15 CRURWWJI-E-CPSSOA-JURID-CONTR        PIC 9(10).
               15 CRURWWJI-E-CTPO-CONTR-NEGOC          PIC 9(03).
               15 CRURWWJI-E-NSEQ-CONTR-NEGOC          PIC 9(10).
               15 CRURWWJI-E-NOCOR-GRP-PPSTA           PIC 9(03).
               15 CRURWWJI-E-CGRP-FLUXO-SUPE           PIC 9(03).
               15 CRURWWJI-E-NVRSAO-GRP-SUPE           PIC 9(02).
               15 CRURWWJI-E-NOCOR-GRP-SUPE            PIC 9(03).
           10 CRURWWJI-BLOCO-SAIDA.
               15 CRURWWJI-S-MAX-OCORR                 PIC 9(05).
               15 CRURWWJI-S-NOCOR-GRP-PGN             PIC 9(03).
               15 CRURWWJI-S-IND-PAGINACAO             PIC X(01).
               15 CRURWWJI-S-REG-SAIDA          OCCURS 15 TIMES.
                  20 CRURWWJI-S-WCONTD-GRP-PPSTA       PIC X(1024).
                  20 CRURWWJI-S-NOCOR-GRP-PPSTA        PIC 9(03).
