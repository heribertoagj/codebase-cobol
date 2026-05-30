      *****************************************************************
      * NOME BOOK : FNEBW3PI - BOOK DE ACESSO AO MODULO               *
      * DESCRICAO : BOOK PARA TRAFEGO DOS DADOS                       *
      *             LISTAR GRUPO DE INFORMACAO DE UMA PROPOSTA        *
      * DATA      : 23/03/2011                                        *
      * AUTOR     : FABIO LOPES BRITO                                 *
      * EMPRESA   : CPM BRAXIS                                        *
      * GRUPO     : CPM BRAXIS                                        *
      * COMPONENTE:                                                   *
      * TAMANHO   : 15519 BYTES                                       *
      *****************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO           *
      *****************************************************************
       05  FNEBW3PI-HEADER.
           10 FNEBW3PI-COD-LAYOUT           PIC X(08)  VALUE 'FNEBW3PI'.
           10 FNEBW3PI-TAM-LAYOUT           PIC 9(05)  VALUE 15519.
       05 FNEBW3PI-REGISTRO.
           10 FNEBW3PI-BLOCO-ENTRADA.
              15 FNEBW3PI-E-MAX-OCORR                 PIC 9(05).
              15 FNEBW3PI-E-NVRSAO-GRP-FLUXO          PIC 9(02).
              15 FNEBW3PI-E-CGRP-INFO-FLUXO           PIC 9(03).
              15 FNEBW3PI-E-CPSSOA-JURID-CONTR        PIC 9(10).
              15 FNEBW3PI-E-CTPO-CONTR-NEGOC          PIC 9(03).
              15 FNEBW3PI-E-NSEQ-CONTR-NEGOC          PIC 9(10).
           10 FNEBW3PI-BLOCO-PAGINACAO.
              15 FNEBW3PI-INDICADOR-PAGINACAO         PIC X(01).
                 88 FNEBW3PI-P-INICIAL                VALUE 'I'.
                 88 FNEBW3PI-P-PRIMEIRA               VALUE 'P'.
                 88 FNEBW3PI-P-SEGUINTE               VALUE 'S'.
                 88 FNEBW3PI-P-ANTERIOR               VALUE 'A'.
                 88 FNEBW3PI-P-ULTIMA                 VALUE 'U'.
              15 FNEBW3PI-CHAVE-INI.
                 20 FNEBW3PI-I-NVRSAO-GRP-FLUXO       PIC 9(02).
                 20 FNEBW3PI-I-CGRP-INFO-FLUXO        PIC 9(03).
                 20 FNEBW3PI-I-CPSSOA-JURID-CONTR     PIC 9(10).
                 20 FNEBW3PI-I-CTPO-CONTR-NEGOC       PIC 9(03).
                 20 FNEBW3PI-I-NSEQ-CONTR-NEGOC       PIC 9(10).
                 20 FNEBW3PI-I-NUMERO-OCC             PIC 9(03).
              15 FNEBW3PI-CHAVE-FIM.
                 20 FNEBW3PI-F-NVRSAO-GRP-FLUXO       PIC 9(02).
                 20 FNEBW3PI-F-CGRP-INFO-FLUXO        PIC 9(03).
                 20 FNEBW3PI-F-CPSSOA-JURID-CONTR     PIC 9(10).
                 20 FNEBW3PI-F-CTPO-CONTR-NEGOC       PIC 9(03).
                 20 FNEBW3PI-F-NSEQ-CONTR-NEGOC       PIC 9(10).
                 20 FNEBW3PI-F-NUMERO-OCC             PIC 9(03).
           10 FNEBW3PI-BLOCO-SAIDA.
              15 FNEBW3PI-S-MAX-OCORR                 PIC 9(05).
              15 FNEBW3PI-S-REG-SAIDA       OCCURS 0 TO 15 TIMES
                 DEPENDING ON FNEBW3PI-S-MAX-OCORR.
                 20 FNEBW3PI-S-NOCOR-GRP-PPSTA        PIC 9(03).
                 20 FNEBW3PI-S-WCONTD-GRP-PPSTA       PIC X(1024).
