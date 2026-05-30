      ******************************************************************
      * NOME BOOK : DCOMW26S - BOOK DE SAIDA                           *
      * DESCRICAO : CONSULTA LISTA DE FORMULARIOS  IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00257 BYTES                                        *
      ******************************************************************
       05 DCOMW26S-HEADER.
          10 DCOMW26S-COD-LAYOUT             PIC X(08) VALUE 'DCOMW26S'.
          10 DCOMW26S-TAM-LAYOUT             PIC 9(05) VALUE 00257.
       05 DCOMW26S-REGISTRO.
          10 DCOMW26S-BLOCO-SAIDA.
             15 DCOMW26S-S1-QTD-FORM         PIC 9(01).
             15 DCOMW26S-S1-FORMS            OCCURS  9 TIMES.
                20 DCOMW26S-INDDOC              PIC X(01).
                20 DCOMW26S-DESCRICAO           PIC X(15).
                20 DCOMW26S-FORMULARIO          PIC X(10).
                20 DCOMW26S-QTDE-VIAS           PIC 9(01).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
