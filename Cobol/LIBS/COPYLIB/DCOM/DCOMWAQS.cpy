      *================================================================*
      * DESCRICAO  : INCLUIR LIBERACAO DA OPERACAO                     *
      * COPYBOOK   : DCOMWAQS - BOOK DE RETORNO DO PROGRAMA FUNCIONAL  *
      * COORDENADOR: DCOM1AQI - INCLUIR LIBERACAO DA OPERACAO          *
      * FLUXO      : DCOMIABO                                          *
      * DATA       : 14/12/2024                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - CAPGEMINI                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWAQS-HEADER.
             10 DCOMWAQS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWAQS'.
             10 DCOMWAQS-TAM-LAYOUT    PIC 9(005)   VALUE 00500.

             10 DCOMWAQS-BLOCO-SAIDA.
      *-->      DADOS DA OPERACAO
      *-->      -----------------
                15 DCOMWAQS-COD-RETORNO             PIC  9(002).
                15 DCOMWAQS-MSG-RETORNO             PIC  X(080).
                15 DCOMWAQS-DANO-OPER-DESC          PIC  9(004).
                15 DCOMWAQS-NSEQ-OPER-DESC          PIC  9(009).
                15 DCOMWAQS-CSIT-DESC-COML          PIC  9(003).
                15 DCOMWAQS-ISIT-DESC-COML          PIC  X(030).
                15 DCOMWAQS-HSIT-DESC-COML          PIC  X(026).
                15 DCOMWAQS-VLIBRC-ANTCP-OPER       PIC  9(015)V9(002).
                15 DCOMWAQS-VLIBRC-DESC-COML        PIC  9(015)V9(002).
                15 DCOMWAQS-HULT-ATULZ              PIC  X(026).
                15 DCOMWAQS-CFUNC-BDSCO             PIC  9(009).
                15 DCOMWAQS-CTERM                   PIC  X(008).
      *-->      DADOS DA LIBERACAO
      *-->      ------------------
                15 DCOMWAQS-CMEIO-LIBRC-DESC        PIC  9(003).
                15 DCOMWAQS-NOPER-MEIO-LIBRC        PIC  9(003).
                15 DCOMWAQS-VLIBRC-DESC-LIBR        PIC  9(015)V9(002).
                15 DCOMWAQS-PLIBRC-DESC-COML        PIC  9(003)V9(003).
                15 DCOMWAQS-DLIBRC-DESC-COML        PIC  X(010).
                15 DCOMWAQS-CINDCD-LIBRC-ANTCP      PIC  X(001).
                15 DCOMWAQS-HULT-ATULZ-LIBRC        PIC  X(026).
                15 DCOMWAQS-CFUNC-BDSCO-LIBRC       PIC  9(009).
                15 DCOMWAQS-CTERM-LIBRC             PIC  X(008).
                15 DCOMWAQS-RESERVA                 PIC  X(173).
