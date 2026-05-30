      ******************************************************************
      * NOME BOOK : DCOMWAFW - INTERFACE DE PERSISTENCIA DO COORDENADOR*
      * DESCRICAO : BUSCAR CONTRATOS ATIVOS DO CLIENTE                 *
      * DATA      : 09/02/2023                                         *
      * EMPRESA   : WIPRO                                              *
      * ANALISTA  : HAMILTON DE GOIS                                   *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 75 BYTES                                           *
      ******************************************************************
      *  DATA       AUTOR             DESCRICAO / MANUTENCAO           *
      ******************************************************************
           05  DCOMWAFW-HEADER.
               10  DCOMWAFW-COD-LAYOUT       PIC X(08) VALUE 'DCOMWAFW'.
               10  DCOMWAFW-TAM-LAYOUT       PIC 9(05) VALUE 75.
           05  DCOMWAFW-REGISTRO.
               10  DCOMWAFW-COD-RETORNO               PIC  9(02).
               10  DCOMWAFW-BLOCO-ENTRADA.
                   15  DCOMWAFW-E-CBCO                PIC  9(03).
                   15  DCOMWAFW-E-CAG-BCRIA           PIC  9(05).
                   15  DCOMWAFW-E-CCTA-BCRIA-CLI      PIC  9(13).
               10  DCOMWAFW-BLOCO-PAGINACAO.
                   15  DCOMWAFW-INDICADOR-PAGINACAO   PIC  X(01).
                       88  DCOMWAFW-P-INICIAL          VALUE 'I'.
                       88  DCOMWAFW-P-PRIMEIRA         VALUE 'P'.
                       88  DCOMWAFW-P-SEGUINTE         VALUE 'S'.
                       88  DCOMWAFW-P-ANTERIOR         VALUE 'A'.
                       88  DCOMWAFW-P-ULTIMA           VALUE 'U'.
                   15  DCOMWAFW-CHAVE-INICIO.
                       20  DCOMWAFW-I-DINIC-OPER-DESC PIC  X(10).
                       20  DCOMWAFW-I-NSEQ-OPER-DESC  PIC  9(09).
                   15  DCOMWAFW-CHAVE-FINAL.
                       20  DCOMWAFW-F-DINIC-OPER-DESC PIC  X(10).
                       20  DCOMWAFW-F-NSEQ-OPER-DESC  PIC  9(09).

