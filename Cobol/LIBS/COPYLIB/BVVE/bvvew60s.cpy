      ******************************************************************
      * NOME BOOK : BVVEW60S - BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTAR LISTA DE CREDENCIADORAS E BANDEIRAS      *
      *             PARA OPTIN                                         *
      * DATA      : DEZEMBRO/2021                                      *
      * AUTOR     : MARCELO DIAS - SISCONSULTORIA                      *
      * TAMANHO   : 24017 BYTES                                        *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW60S-S-CCRDRA-ESTBL-COML : CODIGO CREDENCIADORA            *
      * BVVEW60S-S-IREDZD-CRDRA-ESTBL: DESCRICAO CREDENCIADORA         *
      * BVVEW60S-S-CBANDE-VISAO      : CODIGO BANDEIRA                 *
      ******************************************************************
       05 BVVEW60S-HEADER.
          10 BVVEW60S-COD-LAYOUT            PIC  X(08) VALUE 'BVVEW60S'.
          10 BVVEW60S-TAM-LAYOUT            PIC  9(05) VALUE  24017.
       05 BVVEW60S-S-REGISTRO.
          10 BVVEW60S-S-BLOCO-SAIDA.
             15 BVVEW60S-S-QTD-REG          PIC  9(04).
             15 BVVEW60S-S-AREA-SAIDA.
                20 BVVEW60S-S-TABELA  OCCURS  0 TO 400  TIMES
                   DEPENDING     ON   BVVEW60S-S-QTD-REG.
                   25 BVVEW60S-S-CCRDRA-ESTBL-COML
                                            PIC  9(004).
                   25 BVVEW60S-S-IREDZD-CRDRA-ESTBL
                                            PIC  X(020).
                   25 BVVEW60S-S-TABE OCCURS 12  TIMES.
                      30 BVVEW60S-S-CBANDE-VISAO
                                            PIC  9(003).
