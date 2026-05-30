      ******************************************************************
      * NOME BOOK : DCOMW21S - BOOK DE SAIDA                           *
      * DESCRICAO : LISTA DE SUBPRODUTO            IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 01797 BYTES                                        *
      ******************************************************************
       05 DCOMW21S-HEADER.
          10 DCOMW21S-COD-LAYOUT             PIC X(08) VALUE 'DCOMW21S'.
          10 DCOMW21S-TAM-LAYOUT             PIC 9(05) VALUE 01797.
       05 DCOMW21S-REGISTRO.
          10 DCOMW21S-BLOCO-SAIDA2.
             15 DCOMW21S-S2-QTD-PROD               PIC 9(02).
             15 DCOMW21S-S2-PRODUTO OCCURS 99 TIMES.
                20 DCOMW21S-S2-COD-SBPROD          PIC 9(03).
                20 DCOMW21S-S2-DESC-SBPROD         PIC X(15).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
