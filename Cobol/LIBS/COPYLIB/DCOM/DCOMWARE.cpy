      ******************************************************************
      * NOME BOOK : DCOMWARE - ENTRADA DO COORDENADOR                  *
      * DESCRICAO : OBTER CAMPOS DAS TABELAS DO DCOM                   *
      * DATA      : 03/02/2025                                         *
      * AUTOR     : THIAGO AUGUSTO FELICIANO                           *
      * EMPRESA   : BRADESCO                                           *
      * GRUPO     : PRECIFICACAO                                       *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 00943 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR         DESCRICAO / MANUTENCAO                *
      ******************************************************************
      *   /  /                                                         *
      ******************************************************************
        05 DCOMWARE-HEADER.
           10 DCOMWARE-COD-LAYOUT         PIC X(08)   VALUE 'DCOMWARE'.
           10 DCOMWARE-TAM-LAYOUT         PIC 9(05)   VALUE  00943.
        05 DCOMWARE-REGISTRO.
           10 DCOMWARE-BLOCO-ENTRADA.
               15 DCOMWARE-CAG-BCRIA            PIC  9(05).
               15 DCOMWARE-CCTA-BCRIA-CLI       PIC  9(13).
               15 DCOMWARE-CCONTR-LIM-DESC      PIC  9(09).
               15 DCOMWARE-CVERSAO-CONTR-LIM    PIC  9(03).
               15 DCOMWARE-AREA-CODIGO          OCCURS 100 TIMES.
                 20 DCOMWARE-CODIGO             PIC  9(09).
