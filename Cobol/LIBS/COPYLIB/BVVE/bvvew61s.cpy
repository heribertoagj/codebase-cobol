      ******************************************************************
      * NOME BOOK : BVVEW61S - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTAR LISTA DE BANDEIRAS                       *
      * DATA      : DEZEMBRO/2021                                      *
      * AUTOR     : MARCELO DIAS - SISCONSULTORIA                      *
      * TAMANHO   : 1166 BYTES                                         *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW61S-S-COD-EMPR          : CODIGO BANDEIRA                 *
      * BVVEW61S-S-CBANDE-VISAO      : DESCRICAO BANDEIRA              *
      * BVVEW61S-S-QTD-BAND          : QUANTIDADE DE BANDEIRAS         *
      ******************************************************************
       05 BVVEW61S-HEADER.
          10 BVVEW61S-COD-LAYOUT           PIC  X(08)  VALUE 'BVVEW61S'.
          10 BVVEW61S-TAM-LAYOUT           PIC  9(05)  VALUE 1166.
       05 BVVEW61S-REGISTRO.
          10 BVVEW61S-QTD-BAND             PIC  9(03).
          10 BVVEW61S-BLOCO-SAIDA.
             15 BVVEW61S-TABELA  OCCURS  0 TO   50   TIMES
                DEPENDING    ON    BVVEW61S-QTD-BAND.
                20 BVVEW61S-CBANDE-VISAO PIC  9(03).
                20 BVVEW61S-RBANDE-VISAO PIC  X(20).
