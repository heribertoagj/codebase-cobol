      ******************************************************************
      * DESCRICAO  : INCLUIR CONTRATO LIMITE PARA JORNADA DA AUTOMACAO *
      *              DA DA CONTRATACAO DO CONTRATO LIMITE              *
      * COPYBOOK   : DCOMWACS - BOOK DE SAIDA                          *
      * COORDENADOR: DCOM1ACI - INCLUSAO DO CONTRATO LIMITE            *
      * FLUXO      : DCOMIAAU                                          *
      * DATA       : 15/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      **                                                              **
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWACS-CCONTR-LIM-DESC                                       *
      *                       = CODIGO CONTRATO LIMITE IDENTIFICADO    *
      * DCOMWACS-CVRSAO-CONTR-LIM                                      *
      *                       = CODIGO DA VERSAO DO CONTRATO LIMITE    *
      * DCOMWACS-DINIC-VGCIA-CONTR                                     *
      *                       = DATA DE INICIO DA VIGENCIA DO CONTRATO *
      * DCOMWACS-DVCTO-CONTR-LIM                                       *
      *                       = DATA DE VENCIMENTO DO CONTRATO LIMITE  *
      * DCOMWACS-CCLUB        = CLUB DO CEDENTE DO CONTRATO LIMITE     *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWACS-HEADER.
             10 DCOMWACS-COD-LAYOUT    PIC X(008) VALUE 'DCOMWACS'.
             10 DCOMWACS-TAM-LAYOUT    PIC 9(005) VALUE 00213.
      *
          05 DCOMWACS-REGISTRO.
             10 DCOMWACS-BLOCO-SAIDA.
                15 DCOMWACS-CCONTR-LIM-DESC       PIC  9(009).
                15 DCOMWACS-CVRSAO-CONTR-LIM      PIC  9(003).
                15 DCOMWACS-DINIC-VGCIA-CONTR     PIC  X(010).
                15 DCOMWACS-DVCTO-CONTR-LIM       PIC  X(010).
                15 DCOMWACS-CCLUB                 PIC  9(010).
                15 DCOMWACS-RESERVA               PIC  X(158).
