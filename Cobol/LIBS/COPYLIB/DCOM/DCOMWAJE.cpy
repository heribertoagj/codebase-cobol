      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE CONTRATO LIMITE -LAYOUT ENTRADA*
      * COPYBOOK   : DCOMWAJE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AJL - LISTA DETALHADA DE CONTRATO LIMITE     *
      * FLUXO      : DCOMIAA9                                          *
      * DATA       : 08/08/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAJE-FUNCAO       = IDENTIFICA FILTRO DE PESQUISA PARA LIS-*
      *-> OBRIGATORIO           TAGEM DO CONTRATO LIMITE:              *
      *                         01 - LISTAR POR CLIENTE: AGENCIA/CONTA *
      *                         02 - LISTAR POE CLIENTE: CNPJ / CPF    *
      *                         03 - LISTAR VERSOES DO CONTRATO        *
      *                         04 - DETALHAR CONTRATO ESPEC�FICO      *
      *                         05 - DETALHAR CONTRATO POR OPERACAO    *
      * DCOMWAJE-E-CPRODT-SERVC-OPER                                   *
      *-> OBRIGATORIO         = CODIGO DO PRODUTO CDPS          [OBRIG]*
      * DCOMWAJE-E-CPRODT                                              *
      *-> OBRIGATORIO         = CODIGO DO PRODUTO DCOM          [OBRIG]*
      * DCOMWAJE-E-CSPROD-DESC-COML                                    *
      *-> OBRIGATORIO         = CODIGO DO SUBPRODUTO DCOM       [OBRIG]*
      **  ----------------------------------------------------------  **
      *-> OBRIGATORIO PARA FILTRO DE PESQUISA (DCOMWAJE-FUNCAO = 01) <-*
      *-- ---------------------------------------------------------- --*
      * DCOMWAJE-E-CBCO       = CODIGO DO BANCO [237 - BRADESCO]       *
      * DCOMWAJE-E-CAG-BCRIA  = CODIGO DA AGENCIA DO CLIENTE           *
      * DCOMWAJE-E-CCTA-BCRIA-CLI                                      *
      *                       = CODIGO DA CONTA BANCARIA DO CLIENTE    *
      **  ----------------------------------------------------------  **
      *-> OBRIGATORIO PARA FILTRO DE PESQUISA (DCOMWAJE-FUNCAO = 02) <-*
      *-- ---------------------------------------------------------- --*
      * DCOMWAJE-E-CCNPJ-CPF  = NUMERO DO CNPJ/CPF DO CLIENTE          *
      * DCOMWAJE-E-CFLIAL-CNPJ                                         *
      *                       = NUMERO DA FILIAL DO CNPJ DO CLIENTE    *
      *                         INFORMAR ZEROS EM CASO DE CPF          *
      * DCOMWAJE-E-CCTRL-CNPJ-CPF                                      *
      *                       = CODIGO DO CONTROLE DO CNPJ/CPF CLIENTE *
      **  ----------------------------------------------------------  **
      *-> OBRIGATORIO PARA FILTRO DE PESQUISA (DCOMWAJE-FUNCAO = 03) <-*
      *->                                     (DCOMWAJE-FUNCAO = 04) <-*
      *-- ---------------------------------------------------------- --*
      * DCOMWAJE-E-CCONTR-LIM-DESC                                     *
      *                       = CODIGO DO CONTRATO LIMITE ESPECIFICO   *
      *                         [ENVIAR ZEROS PARA LISTA POR CLIENTE]  *
      * DCOMWAJE-E-CVRSAO-CONTR-LIM                                    *
      *                       = CODIGO DA VERSAO DO CONTRATO LIMITE    *
      *                       -> 000  - PARA LISTA POR CLIENTE OU      *
      *                                 DETALHAR VRS MAIS RECENTE SE   *
      *                                 INFORMADO O CODIGO DO CONTRATO *
      *                                 LIMITE ESPEC�FICO              *
      *-- ---------------------------------------------------------- --*
      *-> OBRIGATORIO PARA FILTRO DE PESQUISA (DCOMWAJE-FUNCAO = 05) <-*
      *-- ---------------------------------------------------------- --*
      * DCOMWAJE-E-DANO-OPER-DESC                                      *
      *                       = IDENTIFICA ANO DE INCLUSAO DA OPERACAO *
      * DCOMWAJE-E-NSEQ-OPER-DESC                                      *
      *                       = IDENTIFICA NUMERO SEQUENCIA DA OPERACAO*
      *                         NO ANO QUE ELA FOI INCLUIDA            *
      *================================================================*
      *
          05 DCOMWAJE-E-HEADER.
             10 DCOMWAJE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAJE'.
             10 DCOMWAJE-TAM-LAYOUT    PIC 9(005) VALUE 00100.
      *
          05 DCOMWAJE-BLOCO-ENTRADA.
             10 DCOMWAJE-FUNCAO                   PIC  9(002).
             10 DCOMWAJE-CPRODT-SERVC-OPER        PIC  9(008).
             10 DCOMWAJE-CPRODT                   PIC  9(003).
             10 DCOMWAJE-CSPROD-DESC-COML         PIC  9(003).
             10 DCOMWAJE-CBCO                     PIC  9(003).
             10 DCOMWAJE-CAG-BCRIA                PIC  9(005).
             10 DCOMWAJE-CCTA-BCRIA-CLI           PIC  9(013).
             10 DCOMWAJE-CCNPJ-CPF                PIC  9(009).
             10 DCOMWAJE-CFLIAL-CNPJ              PIC  9(004).
             10 DCOMWAJE-CCTRL-CNPJ-CPF           PIC  9(002).
             10 DCOMWAJE-CCONTR-LIM-DESC          PIC  9(009).
             10 DCOMWAJE-CVRSAO-CONTR-LIM         PIC  9(003).
             10 DCOMWAJE-DANO-OPER-DESC           PIC  9(004).
             10 DCOMWAJE-NSEQ-OPER-DESC           PIC  9(009).
             10 DCOMWAJE-RESERVA                  PIC  X(010).
