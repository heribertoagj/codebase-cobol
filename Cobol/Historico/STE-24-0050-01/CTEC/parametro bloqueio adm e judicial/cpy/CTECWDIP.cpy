      ******************************************************************
      * NOME BOOK : CTECWDIP                                           *
      * DESCRICAO : BOOK DE PAGINACAO                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * PGM(S)    : CTEC1ATL                                           *
      * TAMANHO   : 00018                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWDIP-HEADER.
              10 CTECWDIP-COD-LAYOUT         PIC X(08) VALUE 'CTECWDIP'.
              10 CTECWDIP-TAM-LAYOUT         PIC 9(05) VALUE 00018.
           05 CTECWDIP-REGISTRO.
              10 CTECWDIP-BLOCO-PAGINACAO.
                 15 CTECWDIP-IND-PAGINACAO   PIC  X(001).
                    88 CTECWDIP-P-INICIAL    VALUE 'I'.
                    88 CTECWDIP-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWDIP-P-SEGUINTE   VALUE 'S'.
                    88 CTECWDIP-P-ANTERIOR   VALUE 'A'.
                    88 CTECWDIP-P-ULTIMA     VALUE 'U'.
                 15 CTECWDIP-PRI-CHAVE.
                    20 CTECWDIP-PRI-CMOVTO-CTA   PIC 9(03).
                 15 CTECWDIP-ULT-CHAVE.
                    20 CTECWDIP-ULT-CMOVTO-CTA   PIC 9(03).
