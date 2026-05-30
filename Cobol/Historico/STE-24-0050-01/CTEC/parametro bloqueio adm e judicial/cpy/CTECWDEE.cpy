      ******************************************************************
      * NOME BOOK : CTECWDEE - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1A1C                                           *
      * TAMANHO   : 00016 BYTES                                        *
      ******************************************************************
           05 CTECWDEE-HEADER.
              10 CTECWDEE-COD-LAYOUT        PIC X(008) VALUE 'CTECWDEE'.
              10 CTECWDEE-TAM-LAYOUT        PIC 9(005) VALUE 00016.
           05 CTECWDEE-REGISTRO.
              10 CTECWDEE-BLOCO-ENTRADA.
                 15 CTECWDEE-E-CPARM-ENCRR-BLOQ   PIC 9(03).
