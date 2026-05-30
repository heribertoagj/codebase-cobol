      ******************************************************************
      * NOME BOOK : DCOMW25L - BOOK DE INTERFACE                       *
      * DESCRICAO : LISTA DE CODIGOS DE DESCONTOS                      *
      * DATA      : 10/10/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00022 BYTES                                        *
      ******************************************************************
       05 DCOMW25L-HEADER.
         10 DCOMW25L-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW25L'.
         10 DCOMW25L-TAM-LAYOUT        PIC 9(05)   VALUE 00022.
       05 DCOMW25L-REGISTRO.
         10 DCOMW25L-BLOCO-ENTRADA.
           15 DCOMW25L-E-CPRODT                    PIC 9(03).
           15 DCOMW25L-E-CSPROD                    PIC 9(03).
         10 DCOMW25L-BLOCO-SAIDA.
           15 DCOMW25L-S-CINDCD-PGDOR-JURO         PIC X(01).
           15 DCOMW25L-S-CINDCD-INCID-IOF          PIC X(01).
           15 DCOMW25L-S-CINDCD-CALC-JURO          PIC X(01).
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
