      *================================================================*
      * DESCRICAO  : LISTA PARAMETROS DE PRODUTOS/SUBPRODUTOS          *
      *              OPERAR NO CANAL TFWEB - LAYOUT DE SAIDA           *
      * COPYBOOK   : DCOMWAIE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AIL - LISTA PARAMETROS DE PRODUTO/SUBPRODUTO *
      *                         APTOS A OPERAR NO CANAL TFWEB          *
      * FLUXO      : DCOMIABH                                          *
      * DATA       : 30/11/2023                                        *
      * AUTOR      : EDGARD ALMEIDA - CAPGEMINI                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAIE-E-CPRODT     = CODIGO DO PRODUTO DCOM                 *
      * DCOMWAIE-E-CSPROD-DESC-COML                                    *
      *                       = CODIGO DO SUBPRODUTO DCOM              *
      **                                                              **
      *================================================================*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWAIE-E-HEADER.
             10 DCOMWAIE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAIE'.
             10 DCOMWAIE-TAM-LAYOUT    PIC 9(005) VALUE 00115.
      *
          05 DCOMWAIE-BLOCO-ENTRADA.
             10 DCOMWAIE-CPRODT                   PIC  9(003).
             10 DCOMWAIE-CSPROD-DESC-COML         PIC  9(003).
             10 DCOMWAIE-RESERVA                  PIC  X(096).
