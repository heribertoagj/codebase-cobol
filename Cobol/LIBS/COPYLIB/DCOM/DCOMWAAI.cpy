      ******************************************************************
      * DESCRICAO  : VERIFICAR GRADE HORARIO DO SISTEMA DCOM           *
      * COPYBOOK   : DCOMWAAI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * FUNCIONAL  : DCOM3AAC - CONSULTAR GRADE HORARIO                *
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
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWAAI-S-SITUACAO-GRADE                                      *
      *                       = SITUACAO DA GRADE HORARIA              *
      *                          01=DISPONIVEL                         *
      *                          02=INDISPONIVEL                       *
      *                          03=INDISPONIVEL PARA O RETROATIVO     *
      *                          04=INDISPONIVEL PARA ULTIMO DIA DO ANO*
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWAAI-HEADER.
             10 DCOMWAAI-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAAI'.
             10 DCOMWAAI-TAM-LAYOUT    PIC 9(005) VALUE 00240.
      *
          05 DCOMWAAI-REGISTRO.
             10 DCOMWAAI-BLOCO-ENTRADA.
                15 DCOMWAAI-E-TP-PROCESS          PIC  X(001).
                15 DCOMWAAI-E-TP-OPCAO            PIC  X(001).
                15 DCOMWAAI-E-TP-FUNCAO           PIC  9(004).
                15 DCOMWAAI-E-RESERVA             PIC  X(081).
      *
             10 DCOMWAAI-BLOCO-SAIDA.
                15 DCOMWAAI-S-SITUACAO-GRADE      PIC  9(002).
                15 DCOMWAAI-S-RESERVA             PIC  X(085).
      *
             10 DCOMWAAI-BLOCO-TRILHA.
                15 DCOMWAAI-T-CCANAL              PIC  9(003).
                15 DCOMWAAI-T-CUSUAR-MANUT        PIC  X(009).
                15 DCOMWAAI-T-RESERVA             PIC  X(041).
