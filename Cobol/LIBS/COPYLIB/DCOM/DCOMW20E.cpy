      ******************************************************************
      * NOME BOOK : DCOMW20E - BOOK DE ENTRADA                         *
      * DESCRICAO : LISTA DE PRODUTO               IMPRESSAO           *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00023 BYTES                                        *
      ******************************************************************
           05 DCOMW20E-HEADER.
              10 DCOMW20E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW20E'.
              10 DCOMW20E-TAM-LAYOUT      PIC 9(05) VALUE 00023.
           05 DCOMW20E-REGISTRO.
              10 DCOMW20E-AREA-ENTRADA.
                 15 DCOMW20E-E-VERSAO               PIC 9(03).
                 15 DCOMW20E-E-IDENTIFI             PIC 9(02).
                 15 DCOMW20E-E-QTD-REGS             PIC 9(05).
