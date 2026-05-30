      ******************************************************************
      * DESCRICAO  : VERIFICAR SE CLIENTE POSSUI CONTRATO LIMITE ATIVO *
      *              PARA JORNADA DE AUTOMACAO DA CONTRATACAO-MOBILE PJ*
      * COPYBOOK   : DCOMWABI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * FUNCIONAL  : DCOM3ABC - VERIFICAR EXISTENCIA DE CONTRATO LIMITE*
      * FLUXO      : DCOMIAAT                                          *
      * DATA       : 02/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWABI-E-CTERM      = IDENTIFICACAO DO CANAL ["MOBILE"]      *
      * DCOMWABI-E-CTPO-DESC-COML                                      *
      *                       = TIPO DE DESCONTO COMERCIAL             *
      *                          [004 = DESCONTO DE ORPAG]             *
      * DCOMWABI-E-CBCO       = CODIGO DO BANCO [237 - BRADESCO]       *
      * DCOMWABI-E-CAG-BCRIA  = CODIGO DA AGENCIA DO CLIENTE           *
      * DCOMWABI-E-CCTA-BCRIA-CLI                                      *
      *                       = CODIGO DA CONTA BANCARIA DO CLIENTE    *
      * DCOMWABI-E-CCNPJ-CPF  = NUMERO DO CNPJ/CPF DO CLIENTE          *
      * DCOMWABI-E-CFLIAL-CNPJ= NUMERO DA FILIAL DO CNPJ DO CLIENTE    *
      *                         INFORMAR ZEROS EM CASO DE CPF          *
      * DCOMWABI-E-CCTRL-CNPJ-CPF                                      *
      *                       = CODIGO DO CONTROLE DO CNPJ/CPF CLIENTE *
      **                                                              **
      ************************* BLOCO DE SAIDA *************************
      **                                                              **
      * DCOMWABI-S-POSSUI-CONTR                                        *
      *                       = IDENTIFICA SE O CLIENTE POSSUI CONTRATO*
      *                          ["S" = SIM / "N" = NAO]               *
      * DCOMWABI-S-CCONTR-LIM-DESC                                     *
      *                       = CODIGO CONTRATO LIMITE IDENTIFICADO    *
      * DCOMWABI-S-CVRSAO-CONTR-LIM                                    *
      *                       = CODIGO DA VERSAO DO CONTRATO LIMITE    *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWABI-HEADER.
             10 DCOMWABI-COD-LAYOUT    PIC X(008) VALUE 'DCOMWABI'.
             10 DCOMWABI-TAM-LAYOUT    PIC 9(005) VALUE 00200.
      *
          05 DCOMWABI-REGISTRO.
             10 DCOMWABI-BLOCO-ENTRADA.
                15 DCOMWABI-E-CTERM               PIC  X(008).
                15 DCOMWABI-E-CTPO-DESC-COML      PIC  9(003).
                15 DCOMWABI-E-CBCO                PIC  9(003).
                15 DCOMWABI-E-CAG-BCRIA           PIC  9(005).
                15 DCOMWABI-E-CCTA-BCRIA-CLI      PIC  9(013).
                15 DCOMWABI-E-CCNPJ-CPF           PIC  9(009).
                15 DCOMWABI-E-CFLIAL-CNPJ         PIC  9(004).
                15 DCOMWABI-E-CCTRL-CNPJ-CPF      PIC  9(002).
      *
             10 DCOMWABI-BLOCO-SAIDA.
                15 DCOMWABI-S-POSSUI-CONTR        PIC  X(001).
                15 DCOMWABI-S-CCONTR-LIM-DESC     PIC  9(009).
                15 DCOMWABI-S-CVRSAO-CONTR-LIM    PIC  9(003).
                15 DCOMWABI-S-RESERVA             PIC  X(074).
      *
             10 DCOMWABI-BLOCO-TRILHA.
                15 DCOMWABI-T-CCANAL              PIC  9(003).
                15 DCOMWABI-T-CUSUAR-MANUT        PIC  X(009).
                15 DCOMWABI-T-RESERVA             PIC  X(041).
