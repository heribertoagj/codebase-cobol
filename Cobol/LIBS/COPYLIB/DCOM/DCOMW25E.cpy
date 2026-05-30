      ******************************************************************
      * NOME BOOK : DCOMW25E - BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTA DETALHE CONTRATO                          *
      * DATA      : 25/09/2017                                         *
      * AUTOR     : SHERLY GUEDES REMEDI                               *
      * EMPRESA   : CD - ALPHA - BRQ                                   *
      * GRUPO     : 494 - CD ALPHA                                     *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00026 BYTES                                        *
      ******************************************************************
           05 DCOMW25E-HEADER.
              10 DCOMW25E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW25E'.
              10 DCOMW25E-TAM-LAYOUT      PIC 9(05) VALUE 00026.
           05 DCOMW25E-REGISTRO.
              10 DCOMW25E-AREA-ENTRADA.
                 15 DCOMW25E-E-DANO-OPER            PIC 9(04).
                 15 DCOMW25E-E-NSEQ-OPER            PIC 9(09).
