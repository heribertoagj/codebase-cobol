      ******************************************************************
      * NOME BOOK : DCOMW27S - BOOK DE SAIDA                           *
      * DESCRICAO : IMPRIME FORMULARIO SELECIONADO IMPRESSAO SAIDA     *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00068 BYTES                                        *
      ******************************************************************
       05 DCOMW27S-HEADER.
          10 DCOMW27S-COD-LAYOUT             PIC X(08) VALUE 'DCOMW27S'.
          10 DCOMW27S-TAM-LAYOUT             PIC 9(05) VALUE 00068.
       05 DCOMW27S-REGISTRO.
          10 DCOMW27S-BLOCO-SAIDA.
             15 DCOMW27S-S-CHAVE                   PIC X(40).
             15 DCOMW27S-S-PROTOCOLO               PIC 9(15).
      ******************************************************************
      *  F I M    D E     B O O K                                      *
      ******************************************************************
