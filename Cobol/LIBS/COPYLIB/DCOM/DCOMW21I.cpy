      ******************************************************************
      * NOME BOOK : DCOMW21I - BOOK DE INTERFACE                       *
      * DESCRICAO : LISTA DE SUBPRODUTO            IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 01810 BYTES                                        *
      ******************************************************************
       05 DCOMW21I-HEADER.
          10 DCOMW21I-COD-LAYOUT             PIC X(08) VALUE 'DCOMW21I'.
          10 DCOMW21I-TAM-LAYOUT             PIC 9(05) VALUE 01810.
       05 DCOMW21I-REGISTRO.
          10 DCOMW21I-ENTRADA.
             15 DCOMW21I-E-VERSAO               PIC 9(03).
             15 DCOMW21I-E-IDENTIFI             PIC 9(02).
             15 DCOMW21I-E-QTD-REGS             PIC 9(05).
             15 DCOMW21I-E-COD-PRODUTO          PIC 9(03).
          10 DCOMW21I-BLOCO-SAIDA2.
             15 DCOMW21I-S2-QTD-PROD               PIC 9(02).
             15 DCOMW21I-S2-PRODUTO OCCURS 99 TIMES.
                20 DCOMW21I-S2-COD-SBPROD          PIC 9(03).
                20 DCOMW21I-S2-DESC-SBPROD         PIC X(15).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
