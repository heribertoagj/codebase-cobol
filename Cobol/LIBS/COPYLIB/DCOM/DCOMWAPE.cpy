      *================================================================*
      * DESCRICAO  : INCLUIR TITULOS SIMULACAO DCOMTFWEB               *
      * COPYBOOK   : DCOMWAPE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1API - INCLUIR TITULOS SIMULACAO DCOMTFWEB    *
      * FLUXO      : DCOMIABF                                          *
      * DATA       : 25/03/2024                                        *
      * AUTOR      : EDGARD ALMEIDA - CAPGEMINI                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      **                                                              **
      *================================================================*
      *
          05 DCOMWAPE-E-HEADER.
             10 DCOMWAPE-COD-LAYOUT    PIC X(008) VALUE 'DCOMWAPE'.
             10 DCOMWAPE-TAM-LAYOUT    PIC 9(005) VALUE 24500.
      *
          05 DCOMWAPE-BLOCO-ENTRADA.
             10 DCOMWAPE-E-DINIC-OPER-DESC        PIC  X(010).
             10 DCOMWAPE-E-CPRODT                 PIC  9(003).
             10 DCOMWAPE-E-CSPROD-DESC-COML       PIC  9(003).
             10 DCOMWAPE-E-CTPO-DESC-COML         PIC  9(003).
             10 DCOMWAPE-E-CBCO                   PIC  9(003).
             10 DCOMWAPE-E-CAG-BCRIA              PIC  9(005).
             10 DCOMWAPE-E-CCTA-BCRIA-CLI         PIC  9(013).
             10 DCOMWAPE-E-CMEIO-ENTRD-DESC       PIC  9(003).
             10 DCOMWAPE-E-NSMULA-OPER-DESC       PIC  9(009).
             10 DCOMWAPE-E-CPPSTA-PRODT           PIC  9(019).
             10 DCOMWAPE-E-HA-MAIS-TITULOS        PIC  X(001).
             10 DCOMWAPE-E-QTDE-TITULOS           PIC  9(003).
             10 DCOMWAPE-E-TITULOS                OCCURS 350.
                20 DCOMWAPE-E-VTITLO-DESC-COML    PIC  9(015)V9(002).
                20 DCOMWAPE-E-CADM-CATAO-CREDT    PIC  9(009).
                20 DCOMWAPE-E-CINDCD-VDA-RCBVL    PIC  X(001).
                20 DCOMWAPE-E-CCNPJ-CPF-TITLO     PIC  9(009).
                20 DCOMWAPE-E-CFLIAL-CNPJ-TITLO   PIC  9(004).
                20 DCOMWAPE-E-CCTRL-CNPJCPF-TITLO PIC  9(002).
                20 DCOMWAPE-E-DVCTO-TITLO-DESC    PIC  X(010).
                20 DCOMWAPE-E-VORPAG-RCBVL        PIC  9(015)V9(002).

             10 DCOMWAPE-E-RESERVA                PIC  X(262).
