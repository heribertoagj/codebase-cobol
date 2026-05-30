      ******************************************************************
      * DESCRICAO  : IDENTIFICAR DE/PARA PRODUTO/SUBPRODUTO DCOM X CDPS*
      * COPYBOOK   : DCOMWAKI - BOOK INTERMEDIARIO                     *
      * COORDENADOR: DCOM1AKC - IDENTIFICAR DE/PARA DCOM X CDPS OU     *
      *                         CDPS X DCOM CONFORME TIPO DE PESQUISA  *
      * FUNCIONAL  : DCOM3AKC                                          *
      * FLUXO      : DCOMIABA                                          *
      * DATA       : 18/09/2023                                        *
      * AUTOR      : EDGARD ALMEIDA -  CAPGEMINI                       *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAKI-E-TPO-PESQUISA        = 'DCOM' OU 'CDPS'              *
      * DCOMWAKI-E-CPRODT-SERVC-OPER   = PRODUTO CDPS                  *
      * DCOMWAKI-E-CPRODT              = PRODUTO DCOM                  *
      * DCOMWAKI-E-CSPROD-DESC-COML    = SUBPRODUTO DCOM               *
      * DCOMEAKI-E-CINDCD-PSSOA-TOMAD  = TIPO DE PESSOA (F/J)          *
      **                                                              **
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWAKS-S-CPRODT-SERVC-OPER   = CODIGO PRODUTO CDPS           *
      * DCOMWAKS-S-IPRODT-SERVC-OPER   = DESCRICAO PRODUTO CDPS        *
      * DCOMWAKS-S-CPRODT              = CODIGO PRODUTO DCOM           *
      * DCOMWAKS-S-IPRODT              = DESCRICAO PRODUTO DCOM        *
      * DCOMWAKS-S-CSPROD-DESC-COML    = CODIGO SUBPRODUTO DCOM        *
      * DCOMWAKS-S-ISPROD-DESC-COML    = DESCRICAO SUBPRODUTO DCOM     *
      * DCOMWAKS-S-CCOOBC-PRODT-DESC   = IDENTIFICACAO DE COOBRIGACAO  *
      * DCOMWAKS-S-CTPO-DESC-COML      = TIPO DE DESCONTO              *
      *                                  [004 - ORPAG]                 *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
      *
          05 DCOMWAKI-HEADER.
             10 DCOMWAKI-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAKI'.
             10 DCOMWAKI-TAM-LAYOUT    PIC 9(005) VALUE 00300.
      *
          05 DCOMWAKI-REGISTRO.
             10 DCOMWAKI-BLOCO-ENTRADA.
                15 DCOMWAKI-E-TPO-PESQUISA           PIC  X(004).
                15 DCOMWAKI-E-CPRODT-SERVC-OPER      PIC  9(008).
                15 DCOMWAKI-E-CPRODT                 PIC  9(003).
                15 DCOMWAKI-E-CSPROD-DESC-COML       PIC  9(003).
                15 DCOMWAKI-E-RESERVA                PIC  X(019).
      *
             10 DCOMWAKI-BLOCO-PAGINACAO.
                15 DCOMWAKI-INDICADOR-PAGINACAO      PIC  X(001).
                   88 DCOMWAKI-P-INICIAL             VALUE 'I'.
                   88 DCOMWAKI-P-PRIMEIRA            VALUE 'P'.
                   88 DCOMWAKI-P-SEGUINTE            VALUE 'S'.
                   88 DCOMWAKI-P-ANTERIOR            VALUE 'A'.
                   88 DCOMWAKI-P-ULTIMA              VALUE 'U'.
                15 DCOMWAPI-CHAVE-INICIO.
                   20 DCOMWAKI-I-CPRODT-SERVC-OPER   PIC  9(008).
                   20 DCOMWAKI-I-CPRODT              PIC  9(003).
                   20 DCOMWAKI-I-CSPROD-DESC-COML    PIC  9(003).
                15 DCOMWAPI-CHAVE-FINAL.
                   20 DCOMWAKI-F-CPRODT-SERVC-OPER   PIC  9(008).
                   20 DCOMWAKI-F-CPRODT              PIC  9(003).
                   20 DCOMWAKI-F-CSPROD-DESC-COML    PIC  9(003).
      *
             10 DCOMWAKI-BLOCO-SAIDA.
                15 DCOMWAKI-S-TPO-PESQUISA           PIC  X(004).
                15 DCOMWAKI-S-CPRODT-SERVC-OPER      PIC  9(008).
                15 DCOMWAKI-S-IPRODT-SERVC-OPER      PIC  X(060).
                15 DCOMWAKI-S-CPRODT                 PIC  9(003).
                15 DCOMWAKI-S-IPRODT                 PIC  X(060).
                15 DCOMWAKI-S-CSPROD-DESC-COML       PIC  9(003).
                15 DCOMWAKI-S-ISPROD-DESC-COML       PIC  X(040).
                15 DCOMWAKI-S-CCOOBC-PRODT-DESC      PIC  X(001).
                15 DCOMWAKI-S-CTPO-DESC-COML         PIC  9(003).
                15 DCOMWAKI-S-CINDCD-PSSOA-TOMAD     PIC  X(001).
                15 DCOMWAKI-S-RESERVA                PIC  X(038).
