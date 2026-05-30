      ******************************************************************
      * NOME BOOK : DCOMWAGE - LAYOUT DE ENTRADA                       *
      * DESCRICAO : CONSULTA DADOS MINIMOS                             *
      * DATA      : 09/02/2023                                         *
      * EMPRESA   : WIPRO                                              *
      * ANALISTA  : HAMILTON DE GOIS                                   *
      * COMPONENTE: DCOM                                               *
      * TAMANHO   : 26 BYTES                                           *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
          05  DCOMWAGE-HEADER.
              10  DCOMWAGE-COD-LAYOUT     PIC  X(08)  VALUE 'DCOMWAGE'.
              10  DCOMWAGE-TAM-LAYOUT     PIC  9(05)  VALUE 26.
          05  DCOMWAGE-REGISTRO.
              10  DCOMWAGE-BLOCO-ENTRADA.
                  15  DCOMWAGE-DANO-OPER-DESC    PIC  9(04).
                  15  DCOMWAGE-NSEQ-OPER-DESC    PIC  9(09).

