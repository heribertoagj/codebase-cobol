      *================================================================*
      * DESCRICAO  : INCLUIR SIMULACAO PROPOSTA TFWEB                  *
      * COPYBOOK   : DCOMWAPI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1API - INCLUIR DADOS PROPOSTA                 *
      * FUNCIONAL  : DCOM3API - INCLUIR DADOS PROPOSTA                 *
      * FLUXO      : DCOMIABF                                          *
      * DATA       : 25/03/2024                                        *
      * AUTOR      : EDGARD ALMEIDA - CAPGEMINI                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *                                                                *
          05 DCOMWAPI-HEADER.
             10 DCOMWAPI-COD-LAYOUT  PIC X(008)   VALUE 'DCOMWAPI'.
             10 DCOMWAPI-TAM-LAYOUT  PIC 9(005)   VALUE 25000.
      *
          05 DCOMWAPI-BLOCO-ENTRADA.
             10 DCOMWAPI-E-DINIC-OPER-DESC        PIC  X(010).
             10 DCOMWAPI-E-CPRODT                 PIC  9(003).
             10 DCOMWAPI-E-CSPROD-DESC-COML       PIC  9(003).
             10 DCOMWAPI-E-CTPO-DESC-COML         PIC  9(003).
             10 DCOMWAPI-E-CBCO                   PIC  9(003).
             10 DCOMWAPI-E-CAG-BCRIA              PIC  9(005).
             10 DCOMWAPI-E-CCTA-BCRIA-CLI         PIC  9(013).
             10 DCOMWAPI-E-CMEIO-ENTRD-DESC       PIC  9(003).
             10 DCOMWAPI-E-NSMULA-OPER-DESC       PIC  9(009).
             10 DCOMWAPI-E-CPPSTA-PRODT           PIC  9(019).
             10 DCOMWAPI-E-HA-MAIS-TITULOS        PIC  X(001).
             10 DCOMWAPI-E-QTDE-TITULOS           PIC  9(003).
             10 DCOMWAPI-E-TITULOS                OCCURS 350.
                20 DCOMWAPI-E-VTITLO-DESC-COML    PIC  9(015)V9(02).
                20 DCOMWAPI-E-CADM-CATAO-CREDT    PIC  9(009).
                20 DCOMWAPI-E-CINDCD-VDA-RCBVL    PIC  X(001).
                20 DCOMWAPI-E-CCNPJ-CPF-TITLO     PIC  9(009).
                20 DCOMWAPI-E-CFLIAL-CNPJ-TITLO   PIC  9(004).
                20 DCOMWAPI-E-CCTRL-CNPJCPF-TITLO PIC  9(002).
                20 DCOMWAPI-E-DVCTO-TITLO-DESC    PIC  X(010).
                20 DCOMWAPI-E-VORPAG-RCBVL        PIC  9(015)V9(02).
      *
      *-->   DADOS DA SESSAO DE LOGIN DO USUARIO
      *
             10 DCOMWAPI-BLOCO-SESSAO.
                15 DCOMWAPI-CCANAL                PIC  9(003).
                15 DCOMWAPI-CTERM                 PIC  X(008).
                15 DCOMWAPI-CAUTEN-SEGRC.
                   20 DCOMWAPI-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWAPI-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWAPI-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWAPI-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWAPI-CODIGO-DEPENDENCIA    PIC  9(005).

             10 DCOMWAPI-E-FILLER                 PIC  X(214).

          05 DCOMWAPI-BLOCO-SAIDA.
             15 DCOMWAPI-S-NSMULA-OPER-DESC       PIC  9(009).
             15 FILLER                            PIC  X(491).
