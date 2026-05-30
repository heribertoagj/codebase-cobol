      ******************************************************************
      * NOME BOOK : DCOMWAHE - LAYOUT DE ENTRADA                       *
      * DESCRICAO : BUSCAR DADOS DO DOCUMENTO DDC                      *
      * DATA      : 21/03/2023                                         *
      * EMPRESA   : CAPGEMINI                                          *
      * ANALISTA  : LEVI BARCIELA DA SILVA CHAVES                      *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 31 BYTES                                           *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
          05  DCOMWAHE-HEADER.
              10  DCOMWAHE-COD-LAYOUT       PIC  X(08) VALUE 'DCOMWAHE'.
              10  DCOMWAHE-TAM-LAYOUT       PIC  9(05) VALUE 31.
          05  DCOMWAHE-REGISTRO.
              10  DCOMWAHE-BLOCO-ENTRADA.
                  15  DCOMWAHE-DANO-OPER-DESC    PIC  9(04).
                  15  DCOMWAHE-NSEQ-OPER-DESC    PIC  9(09).
                  15  DCOMWAHE-NPCELA            PIC  9(05).
