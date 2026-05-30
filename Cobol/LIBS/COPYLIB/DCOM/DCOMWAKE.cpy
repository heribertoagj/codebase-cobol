      ******************************************************************
      * DESCRICAO  : IDENTIFICAR DE/PARA PRODUTO/SUBPRODUTO DCOM X CDPS*
      * COPYBOOK   : DCOMWAKE - BOOK DE ENTRADA                        *
      * COORDENADOR: DCOM1AKC - IDENTIFICAR DE/PARA DCOM X CDPS OU     *
      *                         CDPS X DCOM CONFORME TIPO DE PESQUISA  *
      * FLUXO      : DCOMIABA                                          *
      * DATA       : 18/09/2023                                        *
      * AUTOR      : EDGARD ALMEIDA -  CAPGEMINI                       *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAKE-TPO-PESQUISA          = 'DCOM' OU 'CDPS'              *
      * DCOMWAKE-CPRODT-SERVC-OPER     = PRODUTO CDPS                  *
      * DCOMWAKE-CPRODT                = PRODUTO DCOM                  *
      * DCOMWAKE-CSPROD-DESC-COML      = SUBPRODUTO DCOM               *
      * DCOMEAKE-CINDCD-PSSOA-TOMAD    = TIPO DE PESSOA (F/J)          *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
      *
          05 DCOMWAKE-HEADER.
             10 DCOMWAKE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAKE'.
             10 DCOMWAKE-TAM-LAYOUT    PIC 9(005) VALUE 00050.
      *
          05 DCOMWAKE-REGISTRO.
             10 DCOMWAKE-BLOCO-ENTRADA.
                15 DCOMWAKE-TPO-PESQUISA          PIC  X(004).
                15 DCOMWAKE-CPRODT-SERVC-OPER     PIC  9(008).
                15 DCOMWAKE-CPRODT                PIC  9(003).
                15 DCOMWAKE-CSPROD-DESC-COML      PIC  9(003).
                15 DCOMWAKE-FILLER                PIC  X(019).
