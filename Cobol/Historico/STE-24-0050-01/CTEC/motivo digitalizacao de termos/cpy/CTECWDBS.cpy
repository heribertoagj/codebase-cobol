      ******************************************************************
      * NOME BOOK : CTECWDBS                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * PGM(S)    : CTEC1AKC                                           *
      * TAMANHO   : 03052                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDBS-HEADER.
              10 CTECWDBS-COD-LAYOUT         PIC X(08) VALUE 'CTECWDBS'.
              10 CTECWDBS-TAM-LAYOUT         PIC 9(05) VALUE 00235.
           05 CTECWDBS-REGISTRO.
              10 CTECWDBS-BLOCO-SAIDA.
                 15 CTECWDBS-CMOTVO-PEND-DIGIT       PIC 9(02).
                 15 CTECWDBS-RMOTVO-PEND-DIGIT       PIC X(30).
                 15 CTECWDBS-CUSUAR-INCL             PIC X(09).
                 15 CTECWDBS-RUSUAR-INCL             PIC X(60).
                 15 CTECWDBS-CUSUAR-MANUT            PIC X(09).
                 15 CTECWDBS-RUSUAR-MANUT            PIC X(60).
                 15 CTECWDBS-HINCL-REG               PIC X(26).
                 15 CTECWDBS-HMANUT-REG              PIC X(26).
