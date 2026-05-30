      ******************************************************************
      * NOME BOOK : BVVEW61E - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTAR LISTA DE BANDEIRAS                       *
      *             PARA OPTIN                                         *
      * DATA      : DEZEMBRO/2021                                      *
      * AUTOR     : MARCELO DIAS - SISCONSULTORIA                      *
      * TAMANHO   : 16 BYTES                                           *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW61E-E-COD-EMPR          : SITUACAO BANDEIRA               *
      ******************************************************************
       05 BVVEW61E-HEADER.
          10 BVVEW61E-COD-LAYOUT          PIC  X(08)   VALUE 'BVVEW61E'.
          10 BVVEW61E-TAM-LAYOUT          PIC  9(05)   VALUE  16.
       05 BVVEW61E-E-REGISTRO.
          10 BVVEW61E-E-CTPO-VISAO-BANDE  PIC  9(03).
