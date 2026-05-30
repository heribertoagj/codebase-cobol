      *================================================================*
      * DESCRICAO  : CONSULTAR DADOS DO SUBPRODUTO DCOM APTO A OPERAR  *
      *              NO CANEL TFWEB                                    *
      *          ==> PROCESSO DE CARGA NA COLLECTION MONGODB - PARTE 2 *
      * COPYBOOK   : DCOMWALE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ALC - LISTA DETALHADA DE PRODUTO/SUBPRODUTO  *
      * FLUXO      : DCOMIABJ                                          *
      * DATA       : 22/11/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWALE-CPRODT       = CODIGO DO PRODUTO DCOM                 *
      * DCOMWALE-CSPROD-DESC-COML                                      *
      *                       = CODIGO DO SUBPRODUTO DCOM              *
      **                                                              **
      *================================================================*
      *
          05 DCOMWALE-E-HEADER.
             10 DCOMWALE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWALE'.
             10 DCOMWALE-TAM-LAYOUT    PIC 9(005) VALUE 00100.
      *
          05 DCOMWALE-BLOCO-ENTRADA.
             10 DCOMWALE-CPRODT                   PIC  9(003).
             10 DCOMWALE-CSPROD-DESC-COML         PIC  9(003).
             10 DCOMWALE-RESERVA                  PIC  X(081).
