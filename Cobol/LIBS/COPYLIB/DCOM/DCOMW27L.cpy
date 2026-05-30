      ******************************************************************
      * NOME BOOK : DCOMW27L - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA OPERACAO E GARANTIA                       *
      * DATA      : 24/10/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00206 BYTES                                        *
      ******************************************************************
       05 DCOMW27L-HEADER.
         10 DCOMW27L-COD-LAYOUT              PIC X(08) VALUE 'DCOMW27L'.
         10 DCOMW27L-TAM-LAYOUT                  PIC 9(05) VALUE 00206.
       05 DCOMW27L-REGISTRO.
         10 DCOMW27L-BLOCO-ENTRADA.
           15 DCOMW27L-E-CMEIO-ENTRD-DESC        PIC 9(03).
           15 DCOMW27L-E-CPRODT                  PIC 9(03).
           15 DCOMW27L-E-CSPROD-DESC-COML        PIC 9(03).
         10 DCOMW27L-BLOCO-SAIDA.
           15 DCOMW27L-S-CINDCD-OBRIG-PROMS      PIC X(01).
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
