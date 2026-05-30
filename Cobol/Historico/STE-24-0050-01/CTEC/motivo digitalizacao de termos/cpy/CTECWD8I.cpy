      ******************************************************************
      * NOME BOOK : CTECWD8I - BOOK DE ENCTRADA                        *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * PGM(S)    : CTEC3AGA                                           *
      * TAMANHO   : 00054 BYTES                                        *
      ******************************************************************
           05 CTECWD8I-HEADER.
              10 CTECWD8I-COD-LAYOUT        PIC X(008) VALUE 'CTECWD8I'.
              10 CTECWD8I-TAM-LAYOUT        PIC 9(005) VALUE 00054.
           05 CTECWD8I-REGISTRO.
              10 CTECWD8I-BLOCO-ENTRADA.
                 15 CTECWD8I-E-CMOTVO-PEND-DIGIT     PIC 9(02).
                 15 CTECWD8I-E-RMOTVO-PEND-DIGIT     PIC X(30).
                 15 CTECWD8I-E-CUSUAR-MANUT          PIC X(09).







