      ******************************************************************
      * NOME BOOK : CTECWDCI - BOOK DE ENCTRADA                        *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * PGM(S)    : CTEC3401                                           *
      * TAMANHO   : 00015 BYTES                                        *
      ******************************************************************
           05 CTECWDCI-HEADER.
              10 CTECWDCI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDCI'.
              10 CTECWDCI-TAM-LAYOUT        PIC 9(005) VALUE 00082.
           05 CTECWDCI-BLOCO-ENTRADA.
              10 CTECWDCI-E-CUSUAR          PIC X(09).
           05 CTECWDCI-BLOCO-SAIDA.
              10 CTECWDCI-S-IUSUAR          PIC X(60).