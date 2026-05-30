      ******************************************************************
      * NOME BOOK : DCOMW23S - BOOK DE SAIDA COORDENADOR               *
      * DESCRICAO : CONSULTA INFORMACAO AGENCIA                        *
      * DATA      : 27/09/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 063 BYTES                                          *
      ******************************************************************
       05 DCOMW23S-HEADER.
         10 DCOMW23S-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW23S'.
         10 DCOMW23S-TAM-LAYOUT        PIC 9(05)   VALUE 063.
       05 DCOMW23S-REGISTRO.
         10 DCOMW23S-BLOCO-SAIDA.
           15 DCOMW23S-NOME                        PIC X(50).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
