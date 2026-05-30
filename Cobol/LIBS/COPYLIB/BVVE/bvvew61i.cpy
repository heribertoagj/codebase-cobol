      ******************************************************************
      * NOME BOOK : BVVEW61I - INTERFACE COORDENADOR/FUNCIONAL         *
      * DESCRICAO : CONSULTAR LISTA DE BANDEIRAS                       *
      * DATA      : DEZEMBRO/2021                                      *
      * AUTOR     : MARCELO DIAS - SISCONSULTORIA                      *
      * TAMANHO   : BYTES  = 1169                                      *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      ******************************************************************
       05 BVVEW61I-HEADER.
          10 BVVEW61I-COD-LAYOUT          PIC  X(08) VALUE 'BVVEW61I'.
          10 BVVEW61I-TAM-LAYOUT          PIC  9(05) VALUE  1169.
      *
       05 BVVEW61I-E-REGISTRO.
          10 BVVEW61I-E-CTPO-VISAO-BANDE  PIC  9(03).
      *
       05 BVVEW61I-S-REGISTRO.
          10 BVVEW61I-QTD-BAND            PIC  9(03).
          10 BVVEW61I-S-BLOCO-SAIDA.
             15 BVVEW61I-S-TABELA  OCCURS  0 TO   50    TIMES
                DEPENDING    ON    BVVEW61I-QTD-BAND.
                20 BVVEW61I-S-CBANDE-VISAO   PIC   9(03).
                20 BVVEW61I-S-RBANDE-VISAO   PIC   X(20).
