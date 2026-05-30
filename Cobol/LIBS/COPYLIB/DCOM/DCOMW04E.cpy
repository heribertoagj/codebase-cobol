      ******************************************************************
      * NOME BOOK : DCOMW04E - BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTA DETALHE CONTROLE INTERNO                  *
      * DATA      : 06/10/2017                                         *
      * AUTOR     : ANSELMO DIAS DUARTE                                *
      * EMPRESA   : CD - ALPHA - WIPRO                                 *
      * GRUPO     : 494 - CD ALPHA                                     *
      * TAMANHO   : 026 BYTES                                          *
      ******************************************************************
       05 DCOMW04E-HEADER.
         10 DCOMW04E-COD-LAYOUT        PIC X(08)   VALUE 'DCOMW04E'.
         10 DCOMW04E-TAM-LAYOUT        PIC 9(05)   VALUE 00026.
       05 DCOMW04E-REGISTRO.
         10 DCOMW04E-BLOCO-ENTRADA.
           15 DCOMW04E-E-CONTRATO        PIC 9(13).
      *****************************************************************
      *  F I M    D E     B O O K                                     *
      *****************************************************************
