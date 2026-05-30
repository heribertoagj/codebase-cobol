      ******************************************************************
      * NOME BOOK : DCOMWAFS - LAYOUT DE SAIDA                         *
      * DESCRICAO : BUSCAR CONTRATOS ATIVOS DO CLIENTE                 *
      * DATA      : 09/02/2023                                         *
      * EMPRESA   : WIPRO                                              *
      * ANALISTA  : HAMILTON DE GOIS                                   *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 2537 BYTES                                         *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
          05  DCOMWAFS-HEADER.
              10  DCOMWAFS-COD-LAYOUT       PIC  X(08) VALUE 'DCOMWAFS'.
              10  DCOMWAFS-TAM-LAYOUT       PIC  9(05) VALUE 2537.
          05  DCOMWAFS-REGISTRO.
              10  DCOMWAFS-BLOCO-SAIDA.
                  15  DCOMWAFS-CHAVE-PAGINACAO       PIC  X(19).
                  15  FILLER REDEFINES DCOMWAFS-CHAVE-PAGINACAO.
                      20  DCOMWAFS-DINIC-OPER-DESC-P PIC  X(10).
                      20  DCOMWAFS-NSEQ-OPER-DESC-P  PIC  9(09).

                  15  DCOMWAFS-NUM-CONSULTAS         PIC  9(05).
                  15  DCOMWAFS-REG-SAIDA OCCURS 50 TIMES.
                      20  DCOMWAFS-DANO-OPER-DESC    PIC  9(04).
                      20  DCOMWAFS-NSEQ-OPER-DESC    PIC  9(09).
                      20  DCOMWAFS-DINIC-OPER-DESC   PIC  X(10).
                      20  DCOMWAFS-VOPER-DESC-COML   PIC  9(15)V99.
                      20  DCOMWAFS-IRSUMO-SPROD-DESC PIC  X(10).

