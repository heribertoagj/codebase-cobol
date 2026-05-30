      ******************************************************************
      * NOME BOOK : DCOMW21E - BOOK DE ENTRADA                         *
      * DESCRICAO : LISTA DE SUBPRODUTO            IMPRESSAO           *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00026 BYTES                                        *
      ******************************************************************
           05 DCOMW21E-HEADER.
              10 DCOMW21E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW21E'.
              10 DCOMW21E-TAM-LAYOUT      PIC 9(05) VALUE 00026.
           05 DCOMW21E-REGISTRO.
              10 DCOMW21E-AREA-ENTRADA.
                 15 DCOMW21E-E-VERSAO               PIC 9(03).
                 15 DCOMW21E-E-IDENTIFI             PIC 9(02).
                 15 DCOMW21E-E-QTD-REGS             PIC 9(05).
                 15 DCOMW21E-E-COD-PRODUTO          PIC 9(03).
