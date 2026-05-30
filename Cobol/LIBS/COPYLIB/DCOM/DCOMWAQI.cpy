      *================================================================*
      * DESCRICAO  : LIBERACAO DA OPERACAO                             *
      * COPYBOOK   : DCOMWAQI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AQI - TRATAR LIBERACAO DA OPERACAO           *
      * FLUXO      : DCOMIABO                                          *
      * DATA       : 14/12/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      *-> DCOMWAQI-E-FUNCAO   = INDICA A FUNCIONALIDADE:               *
      *   OBRIGATORIO         - 1 - LIBERACAO - INCLUIR PROTOCOLO      *
      *                       - 2 - LIBERACAO - CONSULTAR PROTOCOLO    *
      *================================================================*
      *
          05 DCOMWAQI-HEADER.
             10 DCOMWAQI-E-COD-LAYOUT   PIC  X(008) VALUE 'DCOMWAQI'.
             10 DCOMWAQI-E-TAM-LAYOUT   PIC  9(005) VALUE 00700.
      *
          05 DCOMWAQI-REGISTRO.
             10 DCOMWAQI-BLOCO-ENTRADA.
                15 DCOMWAQI-E-FUNCAO                PIC  9(001).
                15 DCOMWAQI-E-DANO-OPER-DESC        PIC  9(004).
                15 DCOMWAQI-E-NSEQ-OPER-DESC        PIC  9(009).
                15 DCOMWAQI-E-CTPO-LIBRC            PIC  X(001).
                15 DCOMWAQI-E-VLIBRC                PIC  9(015)V9(002).
                15 DCOMWAQI-E-RESERVA               PIC  X(055).
      *
             10 DCOMWAQI-BLOCO-SESSAO.
                15 DCOMWAQI-CCANAL                  PIC  9(003).
                15 DCOMWAQI-CTERM                   PIC  X(008).
                15 DCOMWAQI-CAUTEN-SEGRC.
                   20 DCOMWAQI-CAUTEN-SEGRC-N       PIC  9(009).
                15 DCOMWAQI-DEPENDENCIA-OPER        PIC  9(005).
                15 DCOMWAQI-EMPR-USUAR-TRAB         PIC  9(010).
                15 DCOMWAQI-DEPEND-USUAR-TRAB       PIC  9(008).
                15 DCOMWAQI-CODIGO-DEPENDENCIA      PIC  9(005).
                15 DCOMWAQI-RESERVA                 PIC  X(052).
      *
             10 DCOMWAQI-BLOCO-SAIDA.
      *-->      DADOS DA OPERACAO
      *-->      -----------------
                15 DCOMWAQI-S-COD-RETORNO           PIC  9(002).
                15 DCOMWAQI-S-MSG-RETORNO           PIC  X(080).
                15 DCOMWAQI-S-DANO-OPER-DESC        PIC  9(004).
                15 DCOMWAQI-S-NSEQ-OPER-DESC        PIC  9(009).
                15 DCOMWAQI-S-CSIT-DESC-COML        PIC  9(003).
                15 DCOMWAQI-S-ISIT-DESC-COML        PIC  X(030).
                15 DCOMWAQI-S-HSIT-DESC-COML        PIC  X(026).
                15 DCOMWAQI-S-VLIBRC-ANTCP-OPER     PIC  9(015)V9(002).
                15 DCOMWAQI-S-VLIBRC-DESC-COML      PIC  9(015)V9(002).
                15 DCOMWAQI-S-HULT-ATULZ            PIC  X(026).
                15 DCOMWAQI-S-CFUNC-BDSCO           PIC  9(009).
                15 DCOMWAQI-S-CTERM                 PIC  X(008).
      *-->      DADOS DA LIBERACAO
      *-->      -------------------
                15 DCOMWAQI-S-CMEIO-LIBRC-DESC      PIC  9(003).
                15 DCOMWAQI-S-NOPER-MEIO-LIBRC      PIC  9(003).
                15 DCOMWAQI-S-VLIBRC-DESC-LIBR      PIC  9(015)V9(002).
                15 DCOMWAQI-S-PLIBRC-DESC-COML      PIC  9(003)V9(003).
                15 DCOMWAQI-S-DLIBRC-DESC-COML      PIC  X(010).
                15 DCOMWAQI-S-CINDCD-LIBRC-ANTCP    PIC  X(001).
                15 DCOMWAQI-S-HULT-ATULZ-LIBRC      PIC  X(026).
                15 DCOMWAQI-S-CFUNC-BDSCO-LIBRC     PIC  9(009).
                15 DCOMWAQI-S-CTERM-LIBRC           PIC  X(008).
                15 DCOMWAQI-S-RESERVA               PIC  X(186).
