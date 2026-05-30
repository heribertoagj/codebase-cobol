      ******************************************************************
      * NOME BOOK : CTECWDAS                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * PGM(S)    : CTEC1AIL                                           *
      * TAMANHO   : 11115                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDAS-HEADER.
              10 CTECWDAS-COD-LAYOUT         PIC X(08) VALUE 'CTECWDAS'.
              10 CTECWDAS-TAM-LAYOUT         PIC 9(05) VALUE 11115.
           05 CTECWDAS-REGISTRO.
              10 CTECWDAS-BLOCO-SAIDA.
                 15 CTECWDAS-NUM-REG      PIC 9(02).
                 15 CTECWDAS-LISTA           OCCURS 0 
                                             TO 50 TIMES
                                             DEPENDING 
                                             ON CTECWDAS-NUM-REG.
                    20 CTECWDAS-CMOTVO-PEND-DIGIT       PIC 9(02).
                    20 CTECWDAS-RMOTVO-PEND-DIGIT       PIC X(30).
                    20 CTECWDAS-CUSUAR-INCL             PIC X(09).
                    20 CTECWDAS-RUSUAR-INCL             PIC X(60).
                    20 CTECWDAS-CUSUAR-MANUT            PIC X(09).
                    20 CTECWDAS-RUSUAR-MANUT            PIC X(60).
                    20 CTECWDAS-HINCL-REG               PIC X(26).
                    20 CTECWDAS-HMANUT-REG              PIC X(26).
