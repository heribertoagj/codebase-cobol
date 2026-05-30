      ******************************************************************
      * NOME BOOK : DCOMW30E - BOOK DE ENTRADA                         *
      * DESCRICAO : CONSULTA DETALHE CONTRATO                          *
      * DATA      : 27/03/2023                                         *
      * AUTOR     : RODRIGO PEREIRA DOS SANTOS                         *
      * EMPRESA   : FOURSYS                                            *
      * COMPONENTE:                                                    *
      * TAMANHO   : 00026 BYTES                                        *
      ******************************************************************
           05 DCOMW30E-HEADER.
              10 DCOMW30E-COD-LAYOUT      PIC X(08) VALUE 'DCOMW30E'.
              10 DCOMW30E-TAM-LAYOUT      PIC 9(05) VALUE 00026.
           05 DCOMW30E-REGISTRO.
              10 DCOMW30E-AREA-ENTRADA.
                 15 DCOMW30E-E-DANO-OPER            PIC 9(04).
                 15 DCOMW30E-E-NSEQ-OPER            PIC 9(09).
