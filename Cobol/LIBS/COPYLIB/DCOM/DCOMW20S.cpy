      ******************************************************************
      * NOME BOOK : DCOMW20S - BOOK DE SAIDA                           *
      * DESCRICAO : LISTA DE PRODUTO               IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 01797 BYTES                                        *
      ******************************************************************
       05 DCOMW20S-HEADER.
          10 DCOMW20S-COD-LAYOUT             PIC X(08) VALUE 'DCOMW20S'.
          10 DCOMW20S-TAM-LAYOUT             PIC 9(05) VALUE 01797.
       05 DCOMW20S-REGISTRO.
          10 DCOMW20S-BLOCO-SAIDA1.
             15 DCOMW20S-S1-QTD-PROD               PIC 9(02).
             15 DCOMW20S-S1-PRODUTO OCCURS      99 TIMES.
                20 DCOMW20S-S1-COD-PRODUTO         PIC 9(03).
                20 DCOMW20S-S1-DESC-PRODUTO        PIC X(15).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
