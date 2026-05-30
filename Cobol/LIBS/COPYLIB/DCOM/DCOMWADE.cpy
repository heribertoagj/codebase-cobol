      ******************************************************************
      * DESCRICAO  : EXCLUSAO DO CONTRATO LIMITE                       *
      * COPYBOOK   : DCOMWADE - BOOK DE ENTRADA                        *
      * COORDENADOR: DCOM1ADE - EXCLUSAO DO CONTRATO LIMITE            *
      * FLUXO      : DCOMIAAV                                          *
      * DATA       : 16/09/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWADE-CTERM        = IDENTIFICACAO DO CANAL ["MOBILE"]      *
      * DCOMWADE-CCONTR-LIM-DESC                                       *
      *                       = CODIGO DO CONTRATO LIMITE              *
      * DCOMWADE-CVRSAO-CONTR-LIM                                      *
      *                       = CODIGO DA VERSAO DO CONTRATO LIMITE    *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWADE-HEADER.
             10 DCOMWADE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWADE'.
             10 DCOMWADE-TAM-LAYOUT    PIC 9(005) VALUE 00100.
      *
          05 DCOMWADE-REGISTRO.
             10 DCOMWADE-BLOCO-ENTRADA.
                15 DCOMWADE-CTERM                 PIC  X(008).
                15 DCOMWADE-CCONTR-LIM-DESC       PIC  9(009).
                15 DCOMWADE-CVRSAO-CONTR-LIM      PIC  9(003).
                15 DCOMWADE-RESERVA               PIC  X(067).
