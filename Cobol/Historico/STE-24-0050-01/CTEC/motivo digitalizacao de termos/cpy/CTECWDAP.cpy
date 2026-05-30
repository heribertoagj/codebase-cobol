      ******************************************************************
      * NOME BOOK : CTECWDAP                                           *
      * DESCRICAO : BOOK DE PAGINACAO                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * PGM(S)    : CTEC1AIL                                           *
      * TAMANHO   : 00018                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDAP-HEADER.
              10 CTECWDAP-COD-LAYOUT         PIC X(08) VALUE 'CTECWDAP'.
              10 CTECWDAP-TAM-LAYOUT         PIC 9(05) VALUE 00018.
           05 CTECWDAP-REGISTRO.
              10 CTECWDAP-BLOCO-PAGINACAO.
                 15 CTECWDAP-IND-PAGINACAO   PIC  X(001).
                    88 CTECWDAP-P-INICIAL    VALUE 'I'.
                    88 CTECWDAP-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWDAP-P-SEGUINTE   VALUE 'S'.
                    88 CTECWDAP-P-ANTERIOR   VALUE 'A'.
                    88 CTECWDAP-P-ULTIMA     VALUE 'U'.
                 15 CTECWDAP-PRI-CHAVE.
                    20 CTECWDAP-PRI-CMOTVO-DIGIT   PIC 9(02).
                 15 CTECWDAP-ULT-CHAVE.
                    20 CTECWDAP-ULT-CMOTVO-DIGIT   PIC 9(02).
