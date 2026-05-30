      ******************************************************************
      * NOME BOOK : DCOMW22L - BOOK DE INTERFACE                       *
      * DESCRICAO : LISTA DE PRODUTO   COMBO IMPRESSAO                 *
      * DATA      : 03/10/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00119 BYTES                                        *
      ******************************************************************
       05 DCOMW22L-HEADER.
         10 DCOMW22L-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW22L'.
         10 DCOMW22L-TAM-LAYOUT        PIC 9(05)   VALUE 00119.
       05 DCOMW22L-REGISTRO.
         10 DCOMW22L-BLOCO-ENTRADA.
           15 DCOMW22L-E-CPRODT                    PIC 9(03).
           15 DCOMW22L-E-CSPRODT                   PIC 9(03).
         10 DCOMW22L-BLOCO-SAIDA.
           15 DCOMW22L-S-ICPROD-DESC             PIC X(40).
           15 DCOMW22L-S-IRSUMO-CPROD            PIC X(10).
           15 DCOMW22L-S-ISPROD-DESC             PIC X(40).
           15 DCOMW22L-S-IRSUMO-SPROD            PIC X(10).
           15 DCOMW22L-S-CTPO-DESC               PIC 9(03).
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
