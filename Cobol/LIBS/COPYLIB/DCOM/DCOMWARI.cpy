      ******************************************************************
      * NOME BOOK : DCOMWARI - ENTRADA DO COORDENADOR                  *
      * DESCRICAO : OBTER CAMPOS DAS TABELAS DO DCOM                   *
      * DATA      : 03/02/2025                                         *
      * AUTOR     : THIAGO AUGUSTO FELICIANO                           *
      * EMPRESA   : BRADESCO                                           *
      * GRUPO     : PRECIFICACAO                                       *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 02205 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR         DESCRICAO / MANUTENCAO                *
      ******************************************************************
      *   /  /                                                         *
      ******************************************************************
        05 DCOMWARI-HEADER.
           10 DCOMWARI-COD-LAYOUT         PIC X(08)   VALUE 'DCOMWARI'.
           10 DCOMWARI-TAM-LAYOUT         PIC 9(05)   VALUE  02205.
        05 DCOMWARI-REGISTRO.
           10 DCOMWARI-BLOCO-ENTRADA.
              15 DCOMWARI-CAG-BCRIA             PIC  9(05).
              15 DCOMWARI-CCTA-BCRIA-CLI        PIC  9(13).
              15 DCOMWARI-CCONTR-LIM-DESC       PIC  9(09).
              15 DCOMWARI-CVERSAO-CONTR-LIM     PIC  9(03).
              15 DCOMWARI-AREA-CODIGO           OCCURS 100 TIMES.
                20 DCOMWARI-CODIGO              PIC  9(09).
           10 DCOMWARI-BLOCO-SAIDA.
              15 DCOMWARI-S-ULT-CONTR-30-DIAS   PIC  9(03).
              15 DCOMWARI-S-VIGEN-CONTRA-MAE    PIC  9(08).
              15 DCOMWARI-S-DATA-ULTIMO-CONTR   PIC  9(08).
              15 DCOMWARI-S-DATA-PRIMEIRO-CONTR PIC  9(08).
              15 DCOMWARI-S-VALOR-TAXA          PIC  9(07)V9(05).
              15 DCOMWARI-S-ULTIMO-VALOR        PIC  9(15)V9(02).
              15 DCOMWARI-S-PRAZO               PIC S9(05).
              15 DCOMWARI-S-FLAG-ANTECIPACAO    PIC  X(01).
              15 DCOMWARI-AREA-COD-FLAG         OCCURS 100 TIMES.
                20  DCOMWARI-S-CODIGO           PIC  9(09).
                20  DCOMWARI-S-FLAG             PIC  X(01).
              15 DCOMWARI-S-ERRO-DB2            PIC  X(200).
