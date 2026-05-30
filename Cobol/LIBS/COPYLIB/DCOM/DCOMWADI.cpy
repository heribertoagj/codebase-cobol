      ******************************************************************
      * DESCRICAO  : EXCLUSAO DO CONTRATO LIMITE                       *
      * COPYBOOK   : DCOMWADI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ADE - EXCLUSAO DO CONTRATO LIMITE            *
      * FLUXO      : DCOMIAAV                                          *
      * DATA       : 16/09/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWADI-E-CTERM      = IDENTIFICACAO DO CANAL ["MOBILE"]      *
      * DCOMWADI-E-CCONTR-LIM-DESC                                     *
      *                       = CODIGO DO CONTRATO LIMITE              *
      * DCOMWADI-E-CVRSAO-CONTR-LIM                                    *
      *                       = CODIGO DA VERSAO DO CONTRATO LIMITE    *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWADI-HEADER.
             10 DCOMWADI-COD-LAYOUT    PIC X(008) VALUE 'DCOMWADI'.
             10 DCOMWADI-TAM-LAYOUT    PIC 9(005) VALUE 00200.
      *
          05 DCOMWADI-REGISTRO.
             10 DCOMWADI-BLOCO-SESSAO.
                15 DCOMWADI-CCANAL                PIC  9(003).
                15 DCOMWADI-CAUTEN-SEGRC.
                   20 DCOMWADI-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWADI-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWADI-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWADI-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWADI-CODIGO-DEPENDENCIA    PIC  9(005).
                15 DCOMWADI-RESERVA               PIC  X(060).
      *
             10 DCOMWADI-BLOCO-ENTRADA.
                15 DCOMWADI-E-CTERM               PIC  X(008).
                15 DCOMWADI-E-CCONTR-LIM-DESC     PIC  9(009).
                15 DCOMWADI-E-CVRSAO-CONTR-LIM    PIC  9(003).
                15 DCOMWADI-E-RESERVA             PIC  X(067).
