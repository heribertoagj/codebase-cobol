      ******************************************************************
      * DESCRICAO  : INCLUSAO DO CONTRATO LIMITE PARA JORNADA DE AUTO- *
      *              MACAO DA CONTRATACAO DO CONTRATO LIMITE           *
      * COPYBOOK   : DCOMWACI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * FUNCIONAL  : DCOM3ACI - INCLUSAO DO CONTRATO LIMITE            *
      * FLUXO      : DCOMIAAU                                          *
      * DATA       : 15/08/2022                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWACI-E-CTERM      = IDENTIFICACAO DO CANAL                 *
      * DCOMWACI-E-CTPO-DESC-COML                                      *
      *                       = TIPO DE DESCONTO COMERCIAL             *
      *                          [004 = DESCONTO DE ORPAG]             *
      * DCOMWACI-E-CBCO       = CODIGO DO BANCO [237 - BRADESCO]       *
      * DCOMWACI-E-CAG-BCRIA  = CODIGO DA AGENCIA DO CLIENTE           *
      * DCOMWACI-E-CCTA-BCRIA-CLI                                      *
      *                       = CODIGO DA CONTA BANCARIA DO CLIENTE    *
      * DCOMWACI-E-CCNPJ-CPF  = NUMERO DO CNPJ/CPF DO CLIENTE          *
      * DCOMWACI-E-CFLIAL-CNPJ                                         *
      *                       = NUMERO DA FILIAL DO CNPJ DO CLIENTE    *
      *                         INFORMAR ZEROS EM CASO DE CPF          *
      * DCOMWACI-E-CINDCD-FORMA-LIQDC                                  *
      *                       = IDENTIFICA A FORMA DE LIQUIDEZ         *
      *                          ["D"= DEBITO EM CONTA / "B"= BOLETO]  *
      * DCOMWACI-E-CINDCD-UTILZ-LIM                                    *
      *                       = IDENTIFICA A UTILIZACAO LIMITE CREDITO *
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWACI-E-CINDCD-DEB-PCIAL                                    *
      *                       = IDENTIFICA DEBITO PARCIAL              *
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWACI-E-CINDCD-TENTV-DEB                                    *
      *                       = IDENTIFICA TENTATIVA DEBITO(TEIMOSINHA)*
      *                          ["S"= SIM / "N" NAO/                  *
      * DCOMWACI-E-DANO-BASE  = CONSULTA DE CREDITO(ANO BASE CONE/CLIM)*
      * DCOMWACI-E-CDEPDC     = CONSULTA DE CREDITO(AGENCIA CONE/CLIM) *
      * DCOMWACI-E-CSEQ-STUDO = CONSULTA DE CREDITO(SEQUENCIA CONE/CLIM*
      * DCOMWACI-E-CCART-LIM-CREDT                                     *
      *                       = CONSULTA DE CREDITO(CARTEIRA CONE/CLIM *
      * DCOMWACI-E-VCONS-NEGOC= CONSULTA DE CREDITO(VL CONSUL CONE/CLIM*
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWACI-E-HEADER.
             10 DCOMWACI-E-COD-LAYOUT  PIC X(008) VALUE 'DCOMWACI'.
             10 DCOMWACI-E-TAM-LAYOUT  PIC 9(005) VALUE 00350.
      *
          05 DCOMWACI-REGISTRO.
             10 DCOMWACI-BLOCO-SESSAO.
                15 DCOMWACI-CCANAL                PIC  9(003).
                15 DCOMWACI-CAUTEN-SEGRC.
                   20 DCOMWACI-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWACI-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWACI-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWACI-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWACI-CODIGO-DEPENDENCIA    PIC  9(005).
                15 DCOMWACI-RESERVA               PIC  X(060).
      *
             10 DCOMWACI-BLOCO-ENTRADA.
                15 DCOMWACI-E-CTERM               PIC  X(008).
                15 DCOMWACI-E-CTPO-DESC-COML      PIC  9(003).
                15 DCOMWACI-E-CBCO                PIC  9(003).
                15 DCOMWACI-E-CAG-BCRIA           PIC  9(005).
                15 DCOMWACI-E-CCTA-BCRIA-CLI      PIC  9(013).
                15 DCOMWACI-E-CCNPJ-CPF           PIC  9(009).
                15 DCOMWACI-E-CFLIAL-CNPJ         PIC  9(004).
                15 DCOMWACI-E-CCTRL-CNPJ-CPF      PIC  9(002).
                15 DCOMWACI-E-CINDCD-FORMA-LIQDC  PIC  X(001).
                15 DCOMWACI-E-CINDCD-UTILZ-LIM    PIC  X(001).
                15 DCOMWACI-E-CINDCD-DEB-PCIAL    PIC  X(001).
                15 DCOMWACI-E-CINDCD-TENTV-DEB    PIC  X(001).
                15 DCOMWACI-E-DANO-BASE           PIC  9(004).
                15 DCOMWACI-E-CDEPDC              PIC  9(005).
                15 DCOMWACI-E-CSEQ-STUDO          PIC  9(009).
                15 DCOMWACI-E-CCART-LIM-CREDT     PIC  X(005).
                15 DCOMWACI-E-VCONS-NEGOC         PIC  9(016)V9(002).
                15 DCOMWACI-E-RESERVA             PIC  X(095).
      *
             10 DCOMWACI-BLOCO-SAIDA.
                15 DCOMWACI-S-CCONTR-LIM-DESC     PIC  9(009).
                15 DCOMWACI-S-CVRSAO-CONTR-LIM    PIC  9(003).
                15 DCOMWACI-S-DINIC-VGCIA-CONTR   PIC  X(010).
                15 DCOMWACI-S-DVCTO-CONTR-LIM     PIC  X(010).
                15 DCOMWACI-S-CCLUB               PIC  9(010).
                15 DCOMWACI-S-RESERVA             PIC  X(158).
