      ******************************************************************
      * NOME BOOK : BVVEW80I - INTERFACE COORDENADOR/FUNCIONAL         *
      * DESCRICAO : VERIFICAR GRADE HORARIA CIP                        *
      * DATA      : JANEIRO/2022                                       *
      * AUTOR     : MARCELO / MARIANI                                  *
      * TAMANHO   : 033 BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                           DESCRICAO                      *
      *----------------------------------------------------------------*
      * CAMPOS USADOS SOMENTE NO FUNCIONAL                             *
      *----------------------------------------------------------------*
       05 BVVEW80I-HEADER.
          10 BVVEW80I-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW80I'.
          10 BVVEW80I-TAM-LAYOUT           PIC  9(05) VALUE  33.

       05 BVVEW80I-REGISTRO.
          10 BVVEW80I-BLOCO-SAIDA.
             15 BVVEW80I-FLAG-GRADE-CIP    PIC  X(01).
             15 BVVEW80I-DATA-PESQUISA     PIC  X(10).
             15 FILLER                     PIC  X(01).
             15 BVVEW80I-HORA-PESQUISA     PIC  X(08).
