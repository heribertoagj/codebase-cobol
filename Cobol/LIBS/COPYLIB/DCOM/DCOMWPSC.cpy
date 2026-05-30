      ******************************************************************
      * DESCRICAO  : SELECAO DE PESSOAS CADASTRADAS NO PSDC ATRAVES DE *
      *              AGENCIA/CONTA, CNPJ/CPF OU CLUB                   *
      * COPYBOOK   : DCOMWPSC - BOOK DE ACESSO AO PROGRAMA BASE        *
      * FUNCIONAL  : DCOM3PSC - CONSULTAR CADASTRO DE CLIENTE NO CADU  *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWPSC-HEADER.
             10 DCOMWPSC-COD-LAYOUT    PIC X(008) VALUE 'DCOMWPSC'.
             10 DCOMWPSC-TAM-LAYOUT    PIC 9(005) VALUE 1000.
      *
          05 DCOMWPSC-REGISTRO.
             10 DCOMWPSC-BLOCO-SESSAO.
                15 DCOMWPSC-CCANAL                PIC  9(003).
                15 DCOMWPSC-CAUTEN-SEGRC.
                   20 DCOMWPSC-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWPSC-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWPSC-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWPSC-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWPSC-CODIGO-DEPENDENCIA    PIC  9(005).
                15 DCOMWPSC-RESERVA               PIC  X(060).
      *
             10 DCOMWPSC-BLOCO-ENTRADA.
                15 DCOMWPSC-E-CCNPJ-CPF           PIC  9(009).
                15 DCOMWPSC-E-CFLIAL-CNPJ         PIC  9(004).
                15 DCOMWPSC-E-CCTRL-CNPJ-CPF      PIC  9(002).
                15 DCOMWPSC-E-CAG-BCRIA           PIC  9(005).
                15 DCOMWPSC-E-CCTA-BCRIA-CLI      PIC  9(007).
                15 DCOMWPSC-E-DCTA-BCRIA-CLI      PIC  X(001).
                15 DCOMWPSC-E-CCLUB               PIC  9(010).
                15 DCOMWPSC-E-CTERM               PIC  X(008).
WIP001          15 DCOMWPSC-E-CINDCD-PSSOA-DESC   PIC  X(002).
WIP001          15 DCOMWPSC-E-CELMTO-DESC-COML    PIC  9(003).
WIP001          15 DCOMWPSC-E-CCHAVE-ELMTO-DESC   PIC  X(045).
WIP001          15 DCOMWPSC-E-RESERVA             PIC  X(042).

             10 DCOMWPSC-BLOCO-SAIDA.
                15 DCOMWPSC-S-CCNPJ-CPF           PIC  9(009).
                15 DCOMWPSC-S-CFLIAL-CNPJ         PIC  9(004).
                15 DCOMWPSC-S-CCTRL-CPF           PIC  9(002).
                15 DCOMWPSC-S-CAG-BCRIA           PIC  9(005).
                15 DCOMWPSC-S-CCTA-BCRIA-CLI      PIC  9(008).
                15 DCOMWPSC-S-CCLUB               PIC  9(010).
                15 DCOMWPSC-S-NM-RAZAO-SOCIAL     PIC  X(070).
                15 DCOMWPSC-S-DT-NASC-FUND        PIC  X(010).
                15 DCOMWPSC-S-CD-IDENT-PESSOA     PIC  X(001).
                15 DCOMWPSC-S-SITUACAO-CLI        PIC  X(020).
                15 DCOMWPSC-S-ELOGDR-PSSOA        PIC  X(070).
                15 DCOMWPSC-S-ELOGDR-NRO          PIC  X(007).
                15 DCOMWPSC-S-RCOMPL-ENDER        PIC  X(030).
                15 DCOMWPSC-S-EBAIRO-ENDER        PIC  X(050).
                15 DCOMWPSC-S-ICIDDE-ENDER        PIC  X(050).
                15 DCOMWPSC-S-CSGL-UF             PIC  X(002).
                15 DCOMWPSC-S-CCEP                PIC  9(005).
                15 DCOMWPSC-S-CCEP-COMPL          PIC  9(003).
                15 DCOMWPSC-S-CDDD                PIC  X(004).
                15 DCOMWPSC-S-CFONE               PIC  9(008).
                15 DCOMWPSC-S-CPORTE-EMPR         PIC  9(003).
                15 DCOMWPSC-S-TPO-CONTA           PIC  9(003).
                15 DCOMWPSC-S-CPSSOA-JURID-CTA    PIC  9(003).
                15 DCOMWPSC-S-CTPO-NEGOC-CTA      PIC  9(003).
                15 DCOMWPSC-S-NSEQ-NEGOC-CTA      PIC  9(010).
                15 DCOMWPSC-S-CLIE-RAZAO          PIC  9(005).
                15 DCOMWPSC-S-COD-TPO-MOVTC       PIC  9(002).
                15 DCOMWPSC-S-CTPO-SGMTO-CLI      PIC  9(003).
                15 DCOMWPSC-S-COD-RATING          PIC  X(002).
                15 DCOMWPSC-S-COD-SIT-CTA         PIC  9(002).
                15 DCOMWPSC-S-RESERVA             PIC  X(345).
