      *================================================================*
      * DESCRICAO  : LISTA PARAMETROS DE PRODUTOS/SUBPRODUTOS          *
      *              OPERAR NO CANAL TFWEB - LAYOUT DE SAIDA           *
      * COPYBOOK   : DCOMWAIS - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AIL - LISTA PARAMETROS DE PRODUTO/SUBPRODUTO *
      *                         APTOS A OPERAR NO CANAL TFWEB          *
      * FLUXO      : DCOMIABH                                          *
      * DATA       : 03/11/2023                                        *
      * AUTOR      : EDGARD ALMEIDA - CAPGEMINI                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWAIS-HEADER.
             10 DCOMWAIS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWAIS'.
             10 DCOMWAIS-TAM-LAYOUT    PIC 9(005)   VALUE 10044.

             10 DCOMWAIS-BLOCO-SAIDA.
      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE PRODUTO DO SISTEMA DCOM                  <--
      *-->  -->    CORRESPONDE A TABELA DCOMB067 - TSPROD_REGRA_CONTR<--
      *-->      ----------------------------------------------------
                15 DCOMWAIS-QTOC-DCOMB067           PIC 9(002).
                15 DCOMWAIS-DCOMB067                OCCURS 002 TIMES.
                   20 DCOMWAIS-B067-CPRODT             PIC 9(003).
                   20 DCOMWAIS-B067-CSPROD-DESC-COML   PIC 9(003).
                   20 DCOMWAIS-B067-HINIC-REGRA-CONT   PIC X(026).
                   20 DCOMWAIS-B067-HFIM-REGRA-CONTR   PIC X(026).
                   20 DCOMWAIS-B067-CINDCD-TOMAD-OPE   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-PSSOA-TOM   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-CONTR-LIM   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-CONVE-CLI   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-CONVE-GRA   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-BAIXA-PCI   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-BAIXA-RTE   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-BAIXA-ANT   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-BAIXA-GST   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-ADTTO-PCE   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-PRROG-PCE   PIC X(001).
                   20 DCOMWAIS-B067-VTARIF-PRROG-PCE   PIC 9(015)V9(02).
                   20 DCOMWAIS-B067-CTARIF-PRROG-PCE   PIC 9(005).
                   20 DCOMWAIS-B067-CRESP-LIM-CREDT    PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-CONS-CRE    PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-ANLSE-CRE   PIC X(001).
                   20 DCOMWAIS-B067-HULT-ATULZ         PIC X(026).
                   20 DCOMWAIS-B067-CINDCD-DISPN-AG    PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-DISPN-NE    PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-DISPN-TLB   PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-DISPN-IB    PIC X(001).
                   20 DCOMWAIS-B067-CINDCD-ADTTO-TX    PIC X(001).
                   20 DCOMWAIS-B067-QVCTO-PCELA-ADT    PIC 9(003).
                   20 DCOMWAIS-B067-CINDCD-ADTTO-OPC   PIC X(001).
                   20 DCOMWAIS-B067-PTX-OPCAO-ACRES    PIC 9(009).
                   20 DCOMWAIS-B067-PTX-OPCAO-REDC     PIC 9(009).
                   20 FILLER                           PIC X(206).

      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE PRODUTO DO SISTEMA DCOM                  <--
      *-->  -->    CORRESPONDE A TABELA DCOMB062 - TSPROD_MEIO_PGTO  <--
      *-->      ----------------------------------------------------
                15 DCOMWAIS-QTOC-DCOMB062           PIC  9(002).
                15 DCOMWAIS-DCOMB062                OCCURS 002 TIMES.
                   20 DCOMWAIS-B062-CTPO-ISENC-DESC   PIC  9(003).
                   20 DCOMWAIS-B062-CPRODT            PIC  9(003).
                   20 DCOMWAIS-B062-CSPROD-DESC-COML  PIC  9(003).
                   20 DCOMWAIS-B062-DINIC-VGCIA-ISE   PIC  X(010).
                   20 DCOMWAIS-B062-DFIM-VGCIA-ISENC  PIC  X(010).
                   20 DCOMWAIS-B062-CINDCD-REST-VLR   PIC  X(001).
                   20 DCOMWAIS-B062-VREST-MAX-OPER    PIC  9(015)V9(02).
                   20 DCOMWAIS-B062-HULT-ATULZ        PIC  X(026).
                   20 DCOMWAIS-B085-ITPO-ISENC-DESC   PIC  X(030).
                   20 DCOMWAIS-B085-IRSUMO-TPO-ISENC  PIC  X(015).
                   20 FILLER                          PIC  X(235).

      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE PRODUTO DO SISTEMA DCOM                  <--
      *-->  -->    CORRESPONDE A TABELA DCOMB066 - TSPROD_MEIO_PGTO  <--
      *-->      ----------------------------------------------------
                15 DCOMWAIS-QTOC-DCOMB066           PIC  9(002).
                15 DCOMWAIS-DCOMB066                OCCURS 010 TIMES.
                   20 DCOMWAIS-B066-CPRODT             PIC 9(003).
                   20 DCOMWAIS-B066-CSPROD-DESC-COML   PIC 9(003).
                   20 DCOMWAIS-B066-CINDCD-VCTO-PCE    PIC X(001).
                   20 DCOMWAIS-B066-CMEIO-PGTO-DESC    PIC 9(003).
                   20 DCOMWAIS-B066-HINIC-MEIO-PGTO    PIC X(026).
                   20 DCOMWAIS-B066-CINDCD-RESP-PGTO   PIC X(001).
                   20 DCOMWAIS-B066-TDEB-PCELA-VENCD   PIC 9(003).
                   20 DCOMWAIS-B066-CINDCD-PRZ-PCELA   PIC X(002).
                   20 DCOMWAIS-B066-HFIM-MEIO-PGTO     PIC X(026).
                   20 DCOMWAIS-B066-HULT-ATULZ         PIC X(026).
                   20 DCOMWAIS-B066-CINDCD-PGTO-OPC    PIC X(001).
                   20 DCOMWAIS-B066-CINDCD-OPCAO-BLE   PIC X(001).
                   20 DCOMWAIS-B090-IMEIO-PGTO-DESC    PIC X(040).
                   20 DCOMWAIS-B090-IRSUMO-MEIO-PGTO   PIC X(015).
                   20 DCOMWAIS-B090-CSGL-MEIO-PGTO     PIC X(006).
                   20 FILLER                           PIC X(196).

      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE SUBPRODUTO DO SISTEMA DCOM -             <--
      *-->  -->    CORRESPONDE A TABELA DCOMB065 - TSPROD_MEIO_LIBRC <--
      *-->      ----------------------------------------------------
                15 DCOMWAIS-QTOT-DCOMB065          PIC  9(002).
                15 DCOMWAIS-DCOMB065               OCCURS 008 TIMES.
                   20 DCOMWAIS-B065-CPRODT            PIC 9(003).
                   20 DCOMWAIS-B065-CSPROD-DESC-COML  PIC 9(003).
                   20 DCOMWAIS-B065-CMEIO-LIBRC-DESC  PIC 9(003).
                   20 DCOMWAIS-B065-HINIC-MEIO-LIBRC  PIC X(026).
                   20 DCOMWAIS-B065-HFIM-MEIO-LIBRC   PIC X(026).
                   20 DCOMWAIS-B065-HULT-ATULZ        PIC X(026).
                   20 DCOMWAIS-B034-IMEIO-LIBRC-DESC  PIC X(025).
                   20 DCOMWAIS-B034-IRSUMO-MEIO-LIBR  PIC X(015).
                   20 DCOMWAIS-B034-CSGL-MEIO-LIBRC   PIC X(006).
                   20 FILLER                          PIC X(220).

      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE SUBPRODUTO DO SISTEMA DCOM -             <--
      *-->  -->    CORRESPONDE A TABELA DCOMB064 - TSPROD_MEIO_LIBRC <--
      *-->      ----------------------------------------------------

                15 DCOMWAIS-QTOT-DCOMB064          PIC  9(002).
                15 DCOMWAIS-DCOMB064               OCCURS 005 TIMES.
                   20 DCOMWAIS-B064-CPRODT             PIC 9(003).
                   20 DCOMWAIS-B064-CSPROD-DESC-COML   PIC 9(003).
                   20 DCOMWAIS-B064-CMEIO-ENTRD-DESC   PIC 9(003).
                   20 DCOMWAIS-B064-HINIC-MEIO-ENTRD   PIC X(026).
                   20 DCOMWAIS-B064-CINDCD-MODLD-DE    PIC X(001).
                   20 DCOMWAIS-B064-HFIM-MEIO-ENTRD    PIC X(026).
                   20 DCOMWAIS-B064-CSIT-DESC-COML     PIC 9(003).
                   20 DCOMWAIS-B064-HSIT-DESC-COML     PIC X(026).
                   20 DCOMWAIS-B064-CINDCD-FORML-OP    PIC X(001).
                   20 DCOMWAIS-B064-CINDCD-LIBRC-OP    PIC X(001).
                   20 DCOMWAIS-B064-CINDCD-LIBRC-ANT   PIC X(001).
                   20 DCOMWAIS-B064-PMAX-LIBRC-ANTCP   PIC 9(003)V9(03).
                   20 DCOMWAIS-B064-TMIN-VCTO-TITLO    PIC 9(005).
                   20 DCOMWAIS-B064-TMAX-VCTO-TITLO    PIC 9(005).
                   20 DCOMWAIS-B064-CINDCD-PRZ-VCTO    PIC X(002).
                   20 DCOMWAIS-B064-TMAX-ACLTO-TITLO   PIC 9(003).
                   20 DCOMWAIS-B064-CINDCD-PRZ-ACLTO   PIC X(002).
                   20 DCOMWAIS-B064-VMAX-DIVRG-TITLO   PIC 9(015)V9(02).
                   20 DCOMWAIS-B064-CINDCD-OBRIG-GAR   PIC X(001).
                   20 DCOMWAIS-B064-CINDCD-OBRIG-PRO   PIC X(001).
                   20 DCOMWAIS-B064-CCART-NORML-DESC   PIC X(005).
                   20 DCOMWAIS-B064-CCART-VENCD-DESC   PIC X(005).
                   20 DCOMWAIS-B064-CCART-DESC-PRODT   PIC X(005).
                   20 DCOMWAIS-B064-CINDCD-TARIF-TR    PIC X(001).
                   20 DCOMWAIS-B064-VTARIF-TRNSF-CA    PIC 9(015)V9(02).
                   20 DCOMWAIS-B064-CTARIF-TRNSF-CA    PIC 9(005).
                   20 DCOMWAIS-B064-CCOBR-TAC-OPER     PIC 9(003).
                   20 DCOMWAIS-B064-VTAC-OPER-DESC     PIC 9(015)V9(02).
                   20 DCOMWAIS-B064-CTAC-OPER-DESC     PIC 9(005).
                   20 DCOMWAIS-B064-CTAC-PERC-OPER     PIC 9(005).
                   20 DCOMWAIS-B064-CTAC-MIN-OPER      PIC 9(005).
                   20 DCOMWAIS-B064-CTAC-MAX-OPER      PIC 9(005).
                   20 DCOMWAIS-B064-CCOBR-TAC-CONTR    PIC 9(003).
                   20 DCOMWAIS-B064-VTAC-OPER-EFETU    PIC 9(015)V9(02).
                   20 DCOMWAIS-B064-CTAC-OPER-EFETU    PIC 9(005).
                   20 DCOMWAIS-B064-CTAC-PERC-CONTR    PIC 9(005).
                   20 DCOMWAIS-B064-CTAC-MIN-CONTR     PIC 9(005).
                   20 DCOMWAIS-B064-CTAC-MAX-CONTR     PIC 9(005).
                   20 DCOMWAIS-B064-HULT-ATULZ         PIC X(026).
                   20 DCOMWAIS-B064-WCTRL-FLUXO-DIGT   PIC X(015).
                   20 DCOMWAIS-B064-CINDCD-TARIF-REG   PIC X(001).
                   20 DCOMWAIS-B064-CINDCD-PRZ-MAX     PIC X(002).
                   20 DCOMWAIS-B033-IMEIO-ENTRD-DESC   PIC X(025).
                   20 DCOMWAIS-B033-IRSUMO-MEIO-ENTR   PIC X(015).
                   20 DCOMWAIS-B033-CSGL-MEIO-ENTRD    PIC X(006).

      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWAIS-RESERVA                 PIC  X(0535).
