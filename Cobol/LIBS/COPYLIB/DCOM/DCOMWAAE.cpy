      ******************************************************************
      * DESCRICAO  : VERIFICAR GRADE HORARIO DO SISTEMA DCOM           *
      * COPYBOOK   : DCOMWAAE - BOOK DE ENTRADA                        *
      * COORDENADOR: DCOM1AAC - CONSULTAR GRADE HORARIO                *
      * FLUXO      : DCOMIAAS                                          *
      * DATA       : 01/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAAI-E-TP-PROCESS = TIPO PROCESSAMENTO CORRENTE DO SISTEMA *
      *                          ["M"=MOVIMENTO / "R"=RETROATIVO]      *
      * DCOMWAAI-E-TP-OPCAO   = TIPO OPCAO                             *
      *                          ["I"=INCLUIR   / "A"=ALTERAR          *
      *                           "C"=CONSULTAR / "E"=EXCLUIR]         *
      * DCOMWAAI-E-TP-FUNCAO  = IDENTIFICA A FUNCIONALIDADE QUE ESTA   *
      *                         SOLICITANDO A VERIFICACAO DA GRADE     *
      *                          [0001 = AUTOMATIZACAO DA CONTRATACAO  *
      *                                  DE CONTRATO LIMITE MOBILE PJ  *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWAAE-HEADER.
             10 DCOMWAAE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAAE'.
             10 DCOMWAAE-TAM-LAYOUT    PIC 9(005) VALUE 00100.
      *
          05 DCOMWAAE-REGISTRO.
             10 DCOMWAAE-BLOCO-ENTRADA.
                15 DCOMWAAE-TP-PROCESS            PIC  X(001).
                15 DCOMWAAE-TP-OPCAO              PIC  X(001).
                15 DCOMWAAE-TP-FUNCAO             PIC  9(004).
                15 DCOMWAAE-RESERVA               PIC  X(081).
