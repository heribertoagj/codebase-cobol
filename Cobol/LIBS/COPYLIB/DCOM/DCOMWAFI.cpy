      ******************************************************************
      * NOME BOOK : DCOMWAFI - BOOK DE INTERFACE                       *
      * DESCRICAO : BUSCAR CONTRATOS ATIVOS DO CLIENTE                 *
      * DATA      : 09/02/2023                                         *
      * AUTOR     : WIPRO                                              *
      * ANALISTA  : HAMILTON DE GOIS                                   *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 2578 BYTES                                         *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05  DCOMWAFI-HEADER.
               10  DCOMWAFI-COD-LAYOUT     PIC  X(08)  VALUE 'DCOMWAFI'.
               10  DCOMWAFI-TAM-LAYOUT     PIC  9(05)  VALUE 2578.
           05  DCOMWAFI-REGISTRO.
               10  DCOMWAFI-BLOCO-ENTRADA.
                   15  DCOMWAFI-E-CBCO                PIC  9(03).
                   15  DCOMWAFI-E-CAG-BCRIA           PIC  9(05).
                   15  DCOMWAFI-E-CCTA-BCRIA-CLI      PIC  9(13).
               10 DCOMWAFI-BLOCO-PAGINACAO.
                   15  DCOMWAFI-INDICADOR-PAGINACAO   PIC  X(01).
                       88  DCOMWAFI-P-INICIAL           VALUE 'I'.
                       88  DCOMWAFI-P-PRIMEIRA          VALUE 'P'.
                       88  DCOMWAFI-P-SEGUINTE          VALUE 'S'.
                       88  DCOMWAFI-P-ANTERIOR          VALUE 'A'.
                       88  DCOMWAFI-P-ULTIMA            VALUE 'U'.
                   15  DCOMWAFI-CHAVE-INICIO.
                       20  DCOMWAFI-I-DINIC-OPER-DESC PIC  X(10).
                       20  DCOMWAFI-I-NSEQ-OPER-DESC  PIC  9(09).
                   15  DCOMWAFI-CHAVE-FINAL.
                       20  DCOMWAFI-F-DINIC-OPER-DESC PIC  X(10).
                       20  DCOMWAFI-F-NSEQ-OPER-DESC  PIC  9(09).
               10  DCOMWAFI-BLOCO-SAIDA.
                   15  DCOMWAFI-S-NUM-CONSULTAS       PIC  9(05).
                   15  DCOMWAFI-S-REG-SAIDA OCCURS 0 TO 50 TIMES
                               DEPENDING ON DCOMWAFI-S-NUM-CONSULTAS.
                       20  DCOMWAFI-S-DANO-OPER-DESC    PIC  9(04).
                       20  DCOMWAFI-S-NSEQ-OPER-DESC    PIC  9(09).
                       20  DCOMWAFI-S-DINIC-OPER-DESC   PIC  X(10).
                       20  DCOMWAFI-S-VOPER-DESC-COML   PIC  9(15)V99.
                       20  DCOMWAFI-S-IRSUMO-SPROD-DESC PIC  X(10).

