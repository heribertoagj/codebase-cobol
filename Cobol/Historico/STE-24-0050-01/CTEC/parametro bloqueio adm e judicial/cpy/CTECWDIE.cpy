      ******************************************************************
      * NOME BOOK : CTECWDIE - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1ASL                                           *
      * TAMANHO   : 00018 BYTES                                        *
      ******************************************************************
           05 CTECWDIE-HEADER.
              10 CTECWDIE-COD-LAYOUT        PIC X(008) VALUE 'CTECWDIE'.
              10 CTECWDIE-TAM-LAYOUT        PIC 9(005) VALUE 00018.
           05 CTECWDIE-REGISTRO.
              10 CTECWDIE-BLOCO-ENTRADA.
                 15 CTECWDIE-E-CMOTVO-SIT-CTA        PIC 9(03).
                 15 CTECWDIE-E-QTD-REG               PIC 9(02).
                 
