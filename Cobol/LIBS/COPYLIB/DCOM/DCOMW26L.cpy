      ******************************************************************
      * NOME BOOK : DCOMW26L - BOOK DE INTERFACE                       *
      * DESCRICAO : CONSULTA LISTA DE FORMULARIOS  IMPRESSAO SAIDA     *
      * DATA      : 18/10/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00027 BYTES                                        *
      ******************************************************************
       05 DCOMW26L-HEADER.
          10 DCOMW26L-COD-LAYOUT             PIC X(08) VALUE 'DCOMW26L'.
          10 DCOMW26L-TAM-LAYOUT             PIC 9(05) VALUE 00027.
       05 DCOMW26L-REGISTRO.
          10 DCOMW26L-ENTRADA.
             15 DCOMW26L-E-DANO-OPER-DESC    PIC 9(04).
             15 DCOMW26L-E-NSEQ-OPER-DESC    PIC 9(09).
          10 DCOMW26L-BLOCO-SAIDA.
             15 DCOMW26L-S-TEM-TITULO        PIC X(01).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
