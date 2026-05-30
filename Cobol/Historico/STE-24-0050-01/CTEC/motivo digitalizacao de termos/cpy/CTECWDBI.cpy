      ******************************************************************
      * NOME BOOK : CTECWDBI - BOOK DE ENCTRADA                        *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * PGM(S)    : CTEC3AKC                                           *
      * TAMANHO   : 00237 BYTES                                        *
      ******************************************************************
           05 CTECWDBI-HEADER.
              10 CTECWDBI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDBI'.
              10 CTECWDBI-TAM-LAYOUT        PIC 9(005) VALUE 00237.
           05 CTECWDBI-REGISTRO.
              10 CTECWDBI-BLOCO-ENTRADA.
                 15 CTECWDBI-E-CMOTVO-PEND-DIGIT     PIC 9(02).
              10 CTECWDBI-BLOCO-SAIDA.
                 15 CTECWDBI-S-CMOTVO-PEND-DIGIT       PIC 9(02).
                 15 CTECWDBI-S-RMOTVO-PEND-DIGIT       PIC X(30).
                 15 CTECWDBI-S-CUSUAR-INCL             PIC X(09).
                 15 CTECWDBI-S-IUSUAR-INCL             PIC X(60).
                 15 CTECWDBI-S-CUSUAR-MANUT            PIC X(09).
                 15 CTECWDBI-S-IUSUAR-MANUT            PIC X(60).
                 15 CTECWDBI-S-HINCL-REG               PIC X(26).
                 15 CTECWDBI-S-HMANUT-REG              PIC X(26).






