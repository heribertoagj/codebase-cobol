      ******************************************************************
      * DESCRICAO  : INCLUIR REGISTRO DA PESSOA EM UM DETERMINADO      *
      *              ELEMENTO (OPERACAO, CONTRATO, ETC)                *
      * COPYBOOK   : DCOMWPSI - BOOK DE ACESSO AO PROGRAMA BASE        *
      * FUNCIONAL  : DCOM3PSI - INCLUIR PESSOA E RELACAO PESSOA        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWPSI-HEADER.
             10 DCOMWPSI-COD-LAYOUT    PIC X(008) VALUE 'DCOMWPSI'.
             10 DCOMWPSI-TAM-LAYOUT    PIC 9(005) VALUE 00800.
      *
          05 DCOMWPSI-REGISTRO.
             10 DCOMWPSI-BLOCO-SESSAO.
                15 DCOMWPSI-CCANAL                PIC  9(003).
                15 DCOMWPSI-CAUTEN-SEGRC.
                   20 DCOMWPSI-CAUTEN-SEGRC-N     PIC  9(009).
                15 DCOMWPSI-DEPENDENCIA-OPER      PIC  9(005).
                15 DCOMWPSI-EMPR-USUAR-TRAB       PIC  9(010).
                15 DCOMWPSI-DEPEND-USUAR-TRAB     PIC  9(008).
                15 DCOMWPSI-CODIGO-DEPENDENCIA    PIC  9(005).
                15 DCOMWPSI-RESERVA               PIC  X(060).
      *
             10 DCOMWPSI-BLOCO-ENTRADA.
                15 DCOMWPSI-E-CCNPJ-CPF           PIC  9(009).
                15 DCOMWPSI-E-CFLIAL-CNPJ         PIC  9(004).
                15 DCOMWPSI-E-CCTRL-CNPJ-CPF      PIC  9(002).
                15 DCOMWPSI-E-CDOCTO-ID           PIC  X(015).
                15 DCOMWPSI-E-IPSSOA-DESC-COML    PIC  X(070).
                15 DCOMWPSI-E-CCLUB               PIC  9(010).
                15 DCOMWPSI-E-CINDCD-PSSOA-DESC   PIC  X(002).
                15 DCOMWPSI-E-CELMTO-DESC-COML    PIC  9(003).
                15 DCOMWPSI-E-CCHAVE-ELMTO-DESC   PIC  X(045).
                15 DCOMWPSI-E-CBCO                PIC  9(003).
                15 DCOMWPSI-E-CAG-BCRIA           PIC  9(005).
                15 DCOMWPSI-E-CCTA-BCRIA-CLI      PIC  9(013).
                15 DCOMWPSI-E-CPOSTO-SERVC        PIC  9(003).
                15 DCOMWPSI-E-IPSSOA-CONJG        PIC  X(040).
                15 DCOMWPSI-E-CCPF-CONJG          PIC  9(009).
                15 DCOMWPSI-E-CCTRL-CPF-CONJG     PIC  9(002).
                15 DCOMWPSI-E-CDOCTO-ID-CONJG     PIC  X(015).
                15 DCOMWPSI-E-CINDCD-CONJG-AUTRZ  PIC  X(001).
                15 DCOMWPSI-E-CCEP                PIC  9(005).
                15 DCOMWPSI-E-CCEP-COMPL          PIC  9(003).
                15 DCOMWPSI-E-ELOGDR              PIC  X(070).
                15 DCOMWPSI-E-NUMERO-LOGRADOURO   PIC  X(007).
                15 DCOMWPSI-E-COMPL-LOGRADOURO    PIC  X(030).
                15 DCOMWPSI-E-IBAIRO              PIC  X(050).
                15 DCOMWPSI-E-IMUN                PIC  X(050).
                15 DCOMWPSI-E-CSGL-UF             PIC  X(002).
                15 DCOMWPSI-E-CTERM               PIC  X(008).
                15 DCOMWPSI-E-RESERVA             PIC  X(091).
      *
             10 DCOMWPSI-BLOCO-SAIDA.
                15 DCOMWPSI-S-NPSSOA-DESC-COML    PIC  9(009).
                15 DCOMWPSI-S-NRELAC-PSSOA-DESC   PIC  9(009).
                15 DCOMWSPI-S-RESERVA             PIC  X(102).
