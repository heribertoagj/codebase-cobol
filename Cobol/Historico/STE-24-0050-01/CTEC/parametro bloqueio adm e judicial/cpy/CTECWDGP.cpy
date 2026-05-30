      ******************************************************************
      * NOME BOOK : CTECWDGP                                           *
      * DESCRICAO : BOOK DE PAGINACAO                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1AGL                                           *
      * TAMANHO   : 00018                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDGP-HEADER.
              10 CTECWDGP-COD-LAYOUT         PIC X(08) VALUE 'CTECWDGP'.
              10 CTECWDGP-TAM-LAYOUT         PIC 9(05) VALUE 00020.
           05 CTECWDGP-REGISTRO.
              10 CTECWDGP-BLOCO-PAGINACAO.
                 15 CTECWDGP-IND-PAGINACAO   PIC  X(001).
                    88 CTECWDGP-P-INICIAL    VALUE 'I'.
                    88 CTECWDGP-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWDGP-P-SEGUINTE   VALUE 'S'.
                    88 CTECWDGP-P-ANTERIOR   VALUE 'A'.
                    88 CTECWDGP-P-ULTIMA     VALUE 'U'.
                 15 CTECWDGP-PRI-CHAVE.
                    20 CTECWDGP-PRI-CPARM-BLOQ   PIC 9(03).
                 15 CTECWDGP-ULT-CHAVE.
                    20 CTECWDGP-ULT-CPARM-BLOQ   PIC 9(03).
