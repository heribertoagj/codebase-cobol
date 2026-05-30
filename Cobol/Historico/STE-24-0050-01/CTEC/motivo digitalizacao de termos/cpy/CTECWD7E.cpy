      ******************************************************************
      * NOME BOOK : CTECWD7E - BOOK DE ENCTRADA                        *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * PGM(S)    : CTEC1AFI                                           *
      * TAMANHO   : 00015 BYTES                                        *
      ******************************************************************
           05 CTECWD7E-HEADER.
              10 CTECWD7E-COD-LAYOUT        PIC X(008) VALUE 'CTECWD7E'.
              10 CTECWD7E-TAM-LAYOUT        PIC 9(005) VALUE 00015.
           05 CTECWD7E-REGISTRO.
              10 CTECWD7E-BLOCO-ENTRADA.
                 15 CTECWD7E-CMOTVO-PEND-DIGIT     PIC 9(02).
                 15 CTECWD7E-RMOTVO-PEND-DIGIT     PIC X(30).
