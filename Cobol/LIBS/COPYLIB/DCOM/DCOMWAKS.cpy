      ******************************************************************
      * DESCRICAO  : VERIFICAR CONSISTENCIAS NECESSARIAS               *
      * COPYBOOK   : DCOMWAKS - BOOK DE ENTRADA                        *
      * COORDENADOR: DCOM1AEC - CONSISTIR - ELEGIBILIDADE              *
      * FLUXO      : DCOMIAA7                                          *
      * DATA       : 18/09/2023                                        *
      * AUTOR      : EDGARD ALMEIDA -  CAPGEMINI                       *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
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
          05 DCOMWAKS-HEADER.
             10 DCOMWAKS-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAKS'.
             10 DCOMWAKS-TAM-LAYOUT    PIC 9(005) VALUE 00250.
      *
          05 DCOMWAKS-REGISTRO.
             10 DCOMWAKS-BLOCO-SAIDA.
                15 DCOMWAKS-TPO-PESQUISA       PIC  X(004).
                15 DCOMWAKS-CPRODT-SERVC-OPER  PIC  9(008).
                15 DCOMWAKS-IPRODT-SERVC-OPER  PIC  X(060).
                15 DCOMWAKS-CPRODT             PIC  9(003).
                15 DCOMWAKS-IPRODT             PIC  X(060).
                15 DCOMWAKS-CSPROD-DESC-COML   PIC  9(003).
                15 DCOMWAKS-ISPROD-DESC-COML   PIC  X(040).
                15 DCOMWAKS-CCOOBC-PRODT-DESC  PIC  X(001).
                15 DCOMWAKS-CTPO-DESC-COML     PIC  9(003).
                15 DCOMWAKS-CINDCD-PSSOA-TOMAD PIC  X(001).
                15 DCOMWAKS-RESERVA            PIC  X(054).
