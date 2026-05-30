      ******************************************************************
      * NOME BOOK : DCOMWARS                                           *
      * DESCRICAO : OBTER CAMPOS NAS TABELAS DO DCOM                   *
      * DATA      : 03/02/2025                                         *
      * AUTOR     : THIAGO AUGUSTO FELICIANO                           *
      * EMPRESA   : BRADESCO                                           *
      * GRUPO     : PRECIFICACAO                                       *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 01275 BYTES                                        *
      ******************************************************************
      * DATA       AUTOR          DESCRICAO / MANUTENCAO               *
      ******************************************************************
      *   /  /                                                         *
      ******************************************************************
        05  DCOMWARS-HEADER.
           10 DCOMWARS-COD-LAYOUT         PIC  X(08)   VALUE 'DCOMWARS'.
           10 DCOMWARS-TAM-LAYOUT         PIC  9(05)   VALUE  01275.
        05    DCOMWARS-BLOCO-SAIDA.
           10 DCOMWARS-S-ULT-CONTR-30-DIAS     PIC  9(03).
           10 DCOMWARS-S-VIGEN-CONTRA-MAE      PIC  9(08).
           10 DCOMWARS-S-DATA-ULTIMO-CONTR     PIC  9(08).
           10 DCOMWARS-S-DATA-PRIMEIRO-CONTR   PIC  9(08).
           10 DCOMWARS-S-VALOR-TAXA            PIC  9(07)V9(05).
           10 DCOMWARS-S-ULTIMO-VALOR          PIC  9(15)V9(02).
           10 DCOMWARS-S-PRAZO                 PIC S9(05).
           10 DCOMWARS-S-FLAG-ANTECIPACAO      PIC  X(01).
           10 DCOMWARS-AREA-COD-FLAG         OCCURS 100 TIMES.
            15  DCOMWARS-S-CODIGO              PIC  9(09).
            15  DCOMWARS-S-FLAG                PIC  X(01).
           10 DCOMWARS-S-ERRO-DB2              PIC  X(200).
