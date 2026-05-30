      ******************************************************************
      * NOME BOOK : DCOMW21L - BOOK DE INTERFACE                       *
      * DESCRICAO : LISTA DE PRODUTO   COMBO IMPRESSAO                 *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 05196 BYTES                                        *
      ******************************************************************
       05 DCOMW21L-HEADER.
         10 DCOMW21L-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW21L'.
         10 DCOMW21L-TAM-LAYOUT        PIC 9(05)   VALUE 05196.
       05 DCOMW21L-REGISTRO.
         10 DCOMW21L-BLOCO-ENTRADA.
           15 DCOMW21L-E-CSIT-DESC                 PIC 9(03).
           15 DCOMW21L-E-QTDE-DESP                 PIC 9(05).
           15 DCOMW21L-E-INSTRUCAO                 PIC X(10).
           15 DCOMW21L-E-CPRODT                    PIC 9(03).
         10 DCOMW21L-BLOCO-SAIDA.
           15 DCOMW21L-S-RESTART                   PIC 9(005).
           15 DCOMW21L-QTDE-RETOR                  PIC 9(002).
           15 DCOMW21L-QTDE-TOTAL                  PIC 9(009).
           15 DCOMW21L-REG-SAIDA         OCCURS 0 TO 99 TIMES
            DEPENDING ON             DCOMW21L-QTDE-RETOR.
             20 DCOMW21L-S-CSPROD-DESC             PIC 9(03).
             20 DCOMW21L-S-ISPROD-DESC             PIC X(40).
             20 DCOMW21L-S-IRSUMO-SPROD            PIC X(10).
      *---------------------------------------------------------------*
      *    F I M     D E    B O O K                                   *
      *---------------------------------------------------------------*
