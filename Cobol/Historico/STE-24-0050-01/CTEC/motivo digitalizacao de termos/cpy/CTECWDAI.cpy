      ******************************************************************
      * NOME BOOK : CTECWDAI - BOOK DE ENCTRADA CTEC1ADL               *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * PGM(S)    : CTEC3AIL                                           *
      * TAMANHO   : 11124 BYTES                                        *
      ******************************************************************
           05 CTECWDAI-HEADER.
              10 CTECWDAI-COD-LAYOUT        PIC X(008) VALUE 'CTECWDAI'.
              10 CTECWDAI-TAM-LAYOUT        PIC 9(005) VALUE 11124.
           05 CTECWDAI-REGISTRO.
              10 CTECWDAI-BLOCO-PAGINACAO.
                 15 CTECWDAI-IND-PAGINACAO   PIC  X(001).
                    88 CTECWDAI-P-INICIAL    VALUE 'I'.
                    88 CTECWDAI-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWDAI-P-SEGUINTE   VALUE 'S'.
                    88 CTECWDAI-P-ANTERIOR   VALUE 'A'.
                    88 CTECWDAI-P-ULTIMA     VALUE 'U'.
                 15 CTECWDAI-PRI-CHAVE.
                    20 CTECWDAI-PRI-CMOTVO-DIGIT   PIC 9(02).
                 15 CTECWDAI-ULT-CHAVE.
                    20 CTECWDAI-ULT-CMOTVO-DIGIT   PIC 9(02).
              10 CTECWDAI-BLOCO-ENTRADA.
                 15 CTECWDAI-E-CMOTVO-PEND-DIGIT   PIC 9(02).
                 15 CTECWDAI-E-QTD-REG             PIC 9(02).
              10 CTECWDAI-BLOCO-SAIDA.
                 15 CTECWDAI-S-NUM-REG      PIC 9(02).
                 15 CTECWDAI-S-LISTA        OCCURS 0 
                                            TO 50 TIMES
                                            DEPENDING 
                                            ON CTECWDAI-S-NUM-REG.
                    20 CTECWDAI-S-CMOTVO-PEND-DIGIT       PIC 9(02).
                    20 CTECWDAI-S-RMOTVO-PEND-DIGIT       PIC X(30).
                    20 CTECWDAI-S-CUSUAR-INCL             PIC X(09).
                    20 CTECWDAI-S-IUSUAR-INCL             PIC X(60).
                    20 CTECWDAI-S-CUSUAR-MANUT            PIC X(09).
                    20 CTECWDAI-S-IUSUAR-MANUT            PIC X(60).
                    20 CTECWDAI-S-HINCL-REG               PIC X(26).
                    20 CTECWDAI-S-HMANUT-REG              PIC X(26).






