      ******************************************************************
      * NOME BOOK : DCOMW23E - BOOK DE ENTRADA COORDENADOR             *
      * DESCRICAO : CONSULTA INFORMACAO AGENCIA                        *
      * DATA      : 27/09/2017                                         *
      * AUTOR     : PATRICK SCARAMAL                                   *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 018 BYTES                                          *
      ******************************************************************
       05 DCOMW23E-HEADER.
         10 DCOMW23E-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW23E'.
         10 DCOMW23E-TAM-LAYOUT        PIC 9(05)   VALUE 018.
       05 DCOMW23E-REGISTRO.
         10 DCOMW23E-BLOCO-ENTRADA.
           15 DCOMW23E-CAG-BCRIA                    PIC 9(05).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
