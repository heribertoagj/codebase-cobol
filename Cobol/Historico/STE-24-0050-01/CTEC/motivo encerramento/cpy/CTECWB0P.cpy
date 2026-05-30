      ******************************************************************
      * NOME BOOK : CTECWB0P                                           *
      * DESCRICAO : BOOK DE PAGINACAO                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * TAMANHO   : 00120                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB0P-HEADER.
              10 CTECWB0P-COD-LAYOUT         PIC X(08) VALUE 'CTECWB0P'.
              10 CTECWB0P-TAM-LAYOUT         PIC 9(05) VALUE 00120.
           05 CTECWB0I-REGISTRO.
              10 CTECWB0P-BLOCO-PAGINACAO.
                 15 CTECWB0P-IND-PAGINACAO   PIC  X(001).
                    88 CTECWB0P-P-INICIAL    VALUE 'I'.
                    88 CTECWB0P-P-PRIMEIRA   VALUE 'P'.
                    88 CTECWB0P-P-SEGUINTE   VALUE 'S'.
                    88 CTECWB0P-P-ANTERIOR   VALUE 'A'.
                    88 CTECWB0P-P-ULTIMA     VALUE 'U'.
                 15 CTECWB0P-PRI-CHAVE.
                    20 CTECWB0P-PRI-CMOTVO-ENCRR   PIC 9(03).
                    20 CTECWB0P-PRI-HINCL-REG      PIC X(26).
                 15 CTECWB0P-ULT-CHAVE.
                    20 CTECWB0P-ULT-CMOTVO-ENCRR   PIC 9(03).
                    20 CTECWB0P-ULT-HINCL-REG      PIC X(26).
