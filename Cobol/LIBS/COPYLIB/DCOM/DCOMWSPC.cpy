      *================================================================*
      * DESCRICAO..: IDENTIFICAR PRODUTO/SUBPRODUTO DCOM CORRESPONDENTE*
      *              AO PRODUTO DCPS INFORMADO                         *
      *           OU IDENTIFICAR PRODUTO CDPS CORRESPONDENTE AO PRODUTO*
      *              /SUBPRODUTO DCOM INFORMADO                        *
      * COPYBOOK   : DCOMWSPC - BOOK DE ACESSO AO PROGRAMA BASE        *
      * FUNCIONAL  : DCOM3SPC - CONSULTAR CADASTRO DE CLIENTE NO CADU  *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWSPC-HEADER.
             10 DCOMWSPC-COD-LAYOUT    PIC  X(008) VALUE 'DCOMWSPC'.
             10 DCOMWSPC-TAM-LAYOUT    PIC  9(005) VALUE 1000.
      *
          05 DCOMWSPC-REGISTRO.
             10 DCOMWSPC-BLOCO-SESSAO.
                15 DCOMWSPC-CCANAL                PIC  9(003).
                15 DCOMWSPC-CAUTEN-SEGRC.
                   20 DCOMWSPC-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWSPC-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWSPC-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWSPC-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWSPC-CODIGO-DEPENDENCIA    PIC  9(005).
                15 DCOMWSPC-RESERVA               PIC  X(060).
      *
             10 DCOMWSPC-BLOCO-ENTRADA.
                15 DCOMWSPC-E-FUNCAO              PIC  9(002).
                15 DCOMWSPC-E-CPRODT-SERVC-OPER   PIC  9(008).
                15 DCOMWSPC-E-CPRODT              PIC  9(003).
                15 DCOMWSPC-E-CSPROD-DESC-COML    PIC  9(003).

                   CNPJ/CPF = J, F , A

                15 DCOMWAJI-S-IPRODT              PIC  X(060).
                15 DCOMWSPC-S-CSPROD-DESC-COML    PIC  9(03).
                15 DCOMWSPC-S-ISPROD-DESC-COML    PIC  X(40).
                15 DCOMWSPC-S-IRSUMO-SPROD-DESC   PIC  X(10).
                15 DCOMWSPC-S-CTPO-DESC-COML      PIC  9(03).
                15 DCOMWSPC-S-IRSUMO-TPO-DESC     PIC  X(10).
                15 DCOMWSPC-S-CINDCD-CONTR-LIM    PIC  X(01).
                15 DCOMWSPC-S-CINDCD-CONVE-CLI    PIC  X(01).
                15 DCOMWSPC-S-CINDCD-CONVE-GRAL   PIC  X(01).
                15 DCOMWSPC-S-CRESP-LIM-CREDT     PIC  X(01).
                15 DCOMWSPC-S-CINDCD-CONS-CREDT   PIC  X(01).
                15 DCOMWSPC-S-CINDCD-ANLSE-CREDT  PIC  X(01).











             10 DCOMWSPC-BLOCO-SAIDA.
                15 DCOMWSPC-S-CCNPJ-CPF           PIC  9(009).
                15 DCOMWSPC-S-CFLIAL-CNPJ         PIC  9(004).
                15 DCOMWSPC-S-CCTRL-CPF           PIC  9(002).
                15 DCOMWSPC-S-CAG-BCRIA           PIC  9(005).
                15 DCOMWSPC-S-CCTA-BCRIA-CLI      PIC  9(008).
                15 DCOMWSPC-S-CCLUB               PIC  9(010).
                15 DCOMWSPC-S-NM-RAZAO-SOCIAL     PIC  X(070).
                15 DCOMWSPC-S-DT-NASC-FUND        PIC  X(010).
                15 DCOMWSPC-S-CD-IDENT-PESSOA     PIC  X(001).
                15 DCOMWSPC-S-SITUACAO-CLI        PIC  X(020).
                15 DCOMWSPC-S-ELOGDR-PSSOA        PIC  X(070).
                15 DCOMWSPC-S-ELOGDR-NRO          PIC  X(007).
                15 DCOMWSPC-S-RCOMPL-ENDER        PIC  X(030).
                15 DCOMWSPC-S-EBAIRO-ENDER        PIC  X(050).
                15 DCOMWSPC-S-ICIDDE-ENDER        PIC  X(050).
                15 DCOMWSPC-S-CSGL-UF             PIC  X(002).
                15 DCOMWSPC-S-CCEP                PIC  9(005).
                15 DCOMWSPC-S-CCEP-COMPL          PIC  9(003).
                15 DCOMWSPC-S-RESERVA             PIC  X(393).
