      ******************************************************************
      * NOME BOOK : CTECWDFE - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1APE                                           *
      * TAMANHO   : 00016 BYTES                                        *
      ******************************************************************
           05 CTECWDFE-HEADER.
              10 CTECWDFE-COD-LAYOUT        PIC X(008) VALUE 'CTECWDFE'.
              10 CTECWDFE-TAM-LAYOUT        PIC 9(005) VALUE 00016.
           05 CTECWDFE-REGISTRO.
              10 CTECWDFE-BLOCO-ENTRADA.
                 15 CTECWDFE-E-CPARM-ENCRR-BLOQ     PIC 9(03).
