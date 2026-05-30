      ******************************************************************
      * NOME BOOK : DCOMWAFE - LAYOUT DE ENTRADA                       *
      * DESCRICAO : BUSCAR CONTRATOS ATIVOS DO CLIENTE                 *
      * DATA      : 09/02/2023                                         *
      * EMPRESA   : WIPRO                                              *
      * ANALISTA  : HAMILTON DE GOIS                                   *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 54 BYTES                                           *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
          05  DCOMWAFE-HEADER.
              10  DCOMWAFE-COD-LAYOUT       PIC  X(08) VALUE 'DCOMWAFE'.
              10  DCOMWAFE-TAM-LAYOUT       PIC  9(05) VALUE 54.
          05  DCOMWAFE-REGISTRO.
              10  DCOMWAFE-BLOCO-ENTRADA.
                  15  DCOMWAFE-CBCO              PIC  9(03).
                  15  DCOMWAFE-CAG-BCRIA         PIC  9(05).
                  15  DCOMWAFE-CCTA-BCRIA-CLI    PIC  9(13).
              10  DCOMWAFE-BLOCO-PAGINACAO.
                  15  DCOMWAFE-INDICADOR-PAGINACAO
                                                 PIC  X(01).
                    88  DCOMWAFE-P-INICIAL       VALUE 'I'.
                    88  DCOMWAFE-P-SEGUINTE      VALUE 'S'.
                  15  DCOMWAFE-CHAVE-PAGINACAO     PIC  X(19).
                  15  FILLER REDEFINES DCOMWAFE-CHAVE-PAGINACAO.
                      20  DCOMWAFE-DINIC-OPER-DESC PIC  X(10).
                      20  DCOMWAFE-NSEQ-OPER-DESC  PIC  9(09).
