      ******************************************************************
      * NOME BOOK : CTECWDFI - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC3APE                                           *
      * TAMANHO   : 00016 BYTES                                        *
      ******************************************************************
           05 CTECWDFI-HEADER.
              10 CTECWDFI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDFI'.
              10 CTECWDFI-TAM-LAYOUT        PIC 9(005) VALUE 00016.
           05 CTECWDFI-REGISTRO.
              10 CTECWDFI-BLOCO-ENTRADA.
                 15 CTECWDFI-E-CPARM-ENCRR-BLOQ        PIC 9(03).
                 15 CTECWDFI-E-CUSUAR-MANUT            PIC X(09).
