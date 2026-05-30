      ******************************************************************
      * NOME BOOK : DCOMW20I - BOOK DE INTERFACE                       *
      * DESCRICAO : LISTA DE PRODUTO               IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 01807 BYTES                                        *
      ******************************************************************
       05 DCOMW20I-HEADER.
          10 DCOMW20I-COD-LAYOUT             PIC X(08) VALUE 'DCOMW20I'.
          10 DCOMW20I-TAM-LAYOUT             PIC 9(05) VALUE 01807.
       05 DCOMW20I-REGISTRO.
          10 DCOMW20I-ENTRADA.
             15 DCOMW20I-E-VERSAO               PIC 9(03).
             15 DCOMW20I-E-IDENTIFI             PIC 9(02).
             15 DCOMW20I-E-QTD-REGS             PIC 9(05).
          10 DCOMW20I-BLOCO-SAIDA1.
             15 DCOMW20I-S1-QTD-PROD               PIC 9(02).
             15 DCOMW20I-S1-PRODUTO OCCURS 99 TIMES.
                20 DCOMW20I-S1-COD-PRODUTO         PIC 9(03).
                20 DCOMW20I-S1-DESC-PRODUTO        PIC X(15).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
