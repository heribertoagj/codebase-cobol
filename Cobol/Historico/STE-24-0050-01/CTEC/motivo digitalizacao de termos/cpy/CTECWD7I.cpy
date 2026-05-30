      ******************************************************************
      * NOME BOOK : CTECWD7I - BOOK DE ENCTRADA                        *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * PGM(S)    : CTEC3AFI                                           *
      * TAMANHO   : 00015 BYTES                                        *
      ******************************************************************
           05 CTECWD7I-HEADER.
              10 CTECWD7I-COD-LAYOUT        PIC X(008) VALUE 'CTECWD7I'.
              10 CTECWD7I-TAM-LAYOUT        PIC 9(005) VALUE 00044.
           05 CTECWD7I-REGISTRO.
              10 CTECWD7I-BLOCO-ENTRADA.
                 15 CTECWD7I-E-CMOTVO-PEND-DIGIT     PIC 9(02).
                 15 CTECWD7I-E-RMOTVO-PEND-DIGIT     PIC X(30).
                 15 CTECWD7I-E-CUSUAR-INCL           PIC X(09).







