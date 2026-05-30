      ******************************************************************
      * NOME BOOK : BVVEW80S - INTERFACE COORDENADOR/FRAMEWORK         *
      *                        BOOK DE SAIDA                           *
      * DESCRICAO : VERIFICAR GRADE HORARIA CIP                        *
      * DATA      : FEVEREIRO/2022                                     *
      * AUTOR     : MARCELO / MARIANI                                  *
      * TAMANHO   : 033 BYTES                                          *
      *----------------------------------------------------------------*
      * CAMPO                          DESCRICAO                       *
      *----------------------------------------------------------------*
      * BVVEW80S-FLAG-GRADE-CIP       : GRADE CIP ABERTA OU FECHADA    *
      *                                 S - SIM - GRADE ABERTA         *
      *                                 N - NAO - GRADE FECHADA        *
      * BVVEW80S-DATA-PESQUISA        : DATA DA PESQUISA               *
      *                                 DD/MM/AAAA                     *
      * BVVEW80S-HORA-PESQUISA        : HORA DA PESQUISA               *
      *                                 HH:MM:SS                       *
      *----------------------------------------------------------------*
       05 BVVEW80S-HEADER.
          10 BVVEW80S-COD-LAYOUT           PIC  X(08) VALUE 'BVVEW80S'.
          10 BVVEW80S-TAM-LAYOUT           PIC  9(05) VALUE 00033.

       05 BVVEW80S-REGISTRO.
          10 BVVEW80S-BLOCO-SAIDA.
             15 BVVEW80S-FLAG-GRADE-CIP    PIC  X(01).
             15 BVVEW80S-DATA-PESQUISA     PIC  X(10).
             15 FILLER                     PIC  X(01).
             15 BVVEW80S-HORA-PESQUISA     PIC  X(08).
