      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE PRODUTOS/SUBPRODUTOS APTOS A   *
      *              OPERAR NO CANAL TFWEB - LAYOUT DE ENTRADA         *
      * COPYBOOK   : DCOMWAEE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AEL - LISTA DETALHADA DE PRODUTO/SUBPRODUTO  *
      * FLUXO      : DCOMIABG                                          *
      * DATA       : 31/11/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAEE-FUNCAO       = IDENTIFICA FILTRO DE PESQUISA PARA LIS-*
      *-> OBRIGATORIO           TAGEM DO PRODUTOS/SUBPRODUTOS, CANAL:  *
      *                         01 - LISTAR TODOS OS PRODUTOS / SUBPRO-*
      *                              DUTOS DISPON�VEIS PARA O TFWEB    *
      *                         02 - LISTAR PRODUTOS E SUBPRODUTOS DCOM*
      *                              DISPON�VEIS PARA UM DETERMIDADO   *
      *                              PRODUTO CDPS INFORMADO            *
      *                         03 - CONSULTAR PRODUTO/SUBPRODUTO DCOM *
      *                              INFORMADO                         *
      *                                                                *
      * DCOMWAEE-E-CPRODT-SERVC-OPER                                   *
      *                       = CODIGO DO PRODUTO CDPS                 *
      *   -> OBRIGATORIO QUANDO DCOMWAEE-FUNCAO = 02, SENAO INFORMAR 0 *
      *                                                                *
      * DCOMWAEE-E-CPRODT     = CODIGO DO PRODUTO DCOM                 *
      *   -> OBRIGATORIO QUANDO DCOMWAEE-FUNCAO = 03, SENAO INFORMAR 0 *
      *                                                                *
      * DCOMWAEE-E-CSPROD-DESC-COML                                    *
      *                       = CODIGO DO SUBPRODUTO DCOM              *
      *   -> OBRIGATORIO QUANDO DCOMWAEE-FUNCAO = 03, SENAO INFORMAR 0 *
      **                                                              **
      *================================================================*
      *
          05 DCOMWAEE-E-HEADER.
             10 DCOMWAEE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAEE'.
             10 DCOMWAEE-TAM-LAYOUT    PIC 9(005) VALUE 00100.
      *
          05 DCOMWAEE-BLOCO-ENTRADA.
             10 DCOMWAEE-FUNCAO                   PIC  9(002).
             10 DCOMWAEE-CPRODT-SERVC-OPER        PIC  9(008).
             10 DCOMWAEE-CPRODT                   PIC  9(003).
             10 DCOMWAEE-CSPROD-DESC-COML         PIC  9(003).
             10 DCOMWAEE-RESERVA                  PIC  X(071).
