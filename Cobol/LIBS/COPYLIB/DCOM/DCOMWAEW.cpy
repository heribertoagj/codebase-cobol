      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE PRODUTO/SUBPRODUTO APTO PARA   *
      *              OPERAR NO CANAL TFWEB - PAGINACAO                 *
      * COPYBOOK   : DCOMWAEW - BOOK DE PAGINACAO PROGRAMA COORDENADOR *
      * COORDENADOR: DCOM3AEL - LISTA DETALHADA DE PRODUTO/SUBPRODUTO  *
      * DATA       : 06/11/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    *
      *================================================================*
      *
          05 DCOMWAEW-HEADER.
             10 DCOMWAEW-COD-LAYOUT  PIC X(008)      VALUE 'DCOMWAEW'.
             10 DCOMWAEW-TAM-LAYOUT  PIC 9(005)      VALUE 00215.
      *
          05 DCOMWAEW-REGISTRO.
             10 DCOMWAEW-COD-RETORNO                 PIC  9(002).
             10 DCOMWAEW-BLOCO-ENTRADA.
                15 DCOMWAEW-FUNCAO                   PIC  9(002).
                15 DCOMWAEW-CPRODT-SERVC-OPER        PIC  9(008).
                15 DCOMWAEW-CPRODT                   PIC  9(003).
                15 DCOMWAEW-CSPROD-DESC-COML         PIC  9(003).
                15 DCOMWAEW-CTPO-DESC-COML           PIC  9(003).
                15 DCOMWAEW-RESERVA                  PIC  X(068).
      *
             10 DCOMWAEW-BLOCO-PAGINACAO.
                15 DCOMWAEW-INDICADOR-PAGINACAO      PIC  X(001).
                   88 DCOMWAEW-P-INICIAL             VALUE 'I'.
                   88 DCOMWAEW-P-PRIMEIRA            VALUE 'P'.
                   88 DCOMWAEW-P-SEGUINTE            VALUE 'S'.
                   88 DCOMWAEW-P-ANTERIOR            VALUE 'A'.
                   88 DCOMWAEW-P-ULTIMA              VALUE 'U'.
               15 DCOMWAEW-CHAVE-INICIO.
                  20 DCOMWAEW-I-CPRODT               PIC  9(003).
                  20 DCOMWAEW-I-CSPROD-DESC-COML     PIC  9(003).
                  20 DCOMWAEW-I-RESERVA              PIC  X(050).
               15 DCOMWAEW-CHAVE-FINAL.
                  20 DCOMWAEW-F-CPRODT               PIC  9(003).
                  20 DCOMWAEW-F-CSPROD-DESC-COML     PIC  9(003).
                  20 DCOMWAEW-F-RESERVA              PIC  X(050).
      *
