      ******************************************************************
      * DESCRICAO  : VERIFICAR SE CLIENTE POSSUI CONTRATO LIMITE ATIVO *
      *              PARA JORNADA DE AUTOMACAO DA CONTRATACAO-MOBILE PJ*
      * COPYBOOK   : DCOMWABE - BOOK DE ENTRADA                        *
      * COORDENADOR: DCOM1ABC - VERIFICAR EXISTENCIA DE CONTRATO LIMITE*
      * FLUXO      : DCOMIAAT                                          *
      * DATA       : 02/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWABE-CTERM        = IDENTIFICACAO DO CTERM ["MOBILE"]      *
      * DCOMWABE-CTPO-DESC-COML                                        *
      *                       = TIPO DE DESCONTO COMERCIAL             *
      *                          [004 = DESCONTO DE ORPAG]             *
      * DCOMWABE-CBCO         = CODIGO DO BANCO [237 - BRADESCO]       *
      * DCOMWABE-CAG-BCRIA    = CODIGO DA AGENCIA DO CLIENTE           *
      * DCOMWABE-CCTA-BCRIA-CLI                                        *
      *                       = CODIGO DA CONTA BANCARIA DO CLIENTE    *
      * DCOMWABE-CCNPJ-CPF    = NUMERO DO CNPJ/CPF DO CLIENTE          *
      * DCOMWABE-CFLIAL-CNPJ= NUMERO DA FILIAL DO CNPJ DO CLIENTE      *
      *                         INFORMAR ZEROS EM CASO DE CPF          *
      * DCOMWABE-CCTRL-CNPJ-CPF                                        *
      *                       = CODIGO DO CONTROLE DO CNPJ/CPF CLIENTE *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWABE-HEADER.
             10 DCOMWABE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWABE'.
             10 DCOMWABE-TAM-LAYOUT    PIC 9(005) VALUE 00060.
      *
          05 DCOMWABE-REGISTRO.
             10 DCOMWABE-BLOCO-ENTRADA.
                15 DCOMWABE-CTERM                 PIC  X(008).
                15 DCOMWABE-CTPO-DESC-COML        PIC  9(003).
                15 DCOMWABE-CBCO                  PIC  9(003).
                15 DCOMWABE-CAG-BCRIA             PIC  9(005).
                15 DCOMWABE-CCTA-BCRIA-CLI        PIC  9(013).
                15 DCOMWABE-CCNPJ-CPF             PIC  9(009).
                15 DCOMWABE-CFLIAL-CNPJ           PIC  9(004).
                15 DCOMWABE-CCTRL-CNPJ-CPF        PIC  9(002).
