      ******************************************************************
      * NOME BOOK : CTECWDGE - BOOK DE ENTRADA                         *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * PGM(S)    : CTEC1AGL                                           *
      * TAMANHO   : 00018 BYTES                                        *
      ******************************************************************
           05 CTECWDGE-HEADER.
              10 CTECWDGE-COD-LAYOUT        PIC X(008) VALUE 'CTECWDGE'.
              10 CTECWDGE-TAM-LAYOUT        PIC 9(005) VALUE 00018.
           05 CTECWDGE-REGISTRO.
              10 CTECWDGE-BLOCO-ENTRADA.
                 15 CTECWDGE-E-CPARM-ENCRR-BLOQ      PIC 9(03).
                 15 CTECWDGE-E-QTD-REG               PIC 9(02).
                 
