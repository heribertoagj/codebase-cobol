      ******************************************************************
      * DESCRICAO  : VERIFICAR SE CLIENTE POSSUI CONTRATO LIMITE ATIVO *
      *              PARA JORNADA DE AUTOMACAO DA CONTRATACAO-MOBILE PJ*
      * COPYBOOK   : DCOMWABS - BOOK DE SAIDA                          *
      * COORDENADOR: DCOM1ABC - VERIFICAR EXISTENCIA DE CONTRATO LIMITE*
      * FLUXO      : DCOMIAAT                                          *
      * DATA       : 02/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWABS-POSSUI-CONTR = IDENTIFICA SE O CLIENTE POSSUI CONTRATO*
      *                          ["S" = SIM / "N" = NAO]               *
      * DCOMWABS-CCONTR-LIM-DESC                                       *
      *                       = CODIGO CONTRATO LIMITE IDENTIFICADO    *
      * DCOMWABS-CVRSAO-CONTR-LIM                                      *
      *                       = CODIGO DA VERSAO DO CONTRATO LIMITE    *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWABS-HEADER.
             10 DCOMWABS-COD-LAYOUT    PIC X(008) VALUE 'DCOMWABS'.
             10 DCOMWABS-TAM-LAYOUT    PIC 9(005) VALUE 00100.
      *
          05 DCOMWABS-REGISTRO.
             10 DCOMWABS-BLOCO-SAIDA.
                15 DCOMWABS-POSSUI-CONTR          PIC  X(001).
                15 DCOMWABS-CCONTR-LIM-DESC       PIC  9(009).
                15 DCOMWABS-CVRSAO-CONTR-LIM      PIC  9(003).
                15 DCOMWABS-RESERVA               PIC  X(074).
