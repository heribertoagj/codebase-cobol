      ******************************************************************
      * DESCRICAO  : INCLUIR REGISTROS DE LOG COMPLETO OU RESUMIDO DO  *
      *              SISTEMA DCOM                                      *
      * COPYBOOK   : DCOMWLGI - BOOK DE ACESSO AO PROGRAMA BASE        *
      * FUNCIONAL  : DCOM3LGI - INCLUIR PESSOA E RELACAO PESSOA        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWLGI-HEADER.
             10 DCOMWLGI-COD-LAYOUT    PIC X(008) VALUE 'DCOMWLGI'.
             10 DCOMWLGI-TAM-LAYOUT    PIC 9(005) VALUE 03312.
      *
          05 DCOMWLGI-REGISTRO.
             10 DCOMWLGI-BLOCO-SESSAO.
                15 DCOMWLGI-CCANAL                   PIC  9(003).
                15 DCOMWLGI-CAUTEN-SEGRC.
                   20 DCOMWLGI-CAUTEN-SEGRC-N        PIC  9(009).
                15 DCOMWLGI-DEPENDENCIA-OPER         PIC  9(005).
                15 DCOMWLGI-EMPR-USUAR-TRAB          PIC  9(010).
                15 DCOMWLGI-DEPEND-USUAR-TRAB        PIC  9(008).
                15 DCOMWLGI-CODIGO-DEPENDENCIA       PIC  9(005).
                15 DCOMWLGI-RESERVA                  PIC  X(060).
      *
             10 DCOMWLGI-BLOCO-ENTRADA.
                15 DCOMWLGI-E-CTERM                  PIC  X(008).
                15 DCOMWLGI-E-CELMTO-DESC-COML       PIC  9(003).
                15 DCOMWLGI-E-CEVNTO-DESC-COML       PIC  9(003).
                15 DCOMWLGI-E-HULT-ATULZ             PIC  X(026).
                15 DCOMWLGI-E-CINDCD-MANUT-REG       PIC  X(001).
                15 DCOMWLGI-E-CCHAVE-ELMTO-DESC      PIC  X(045).
                15 DCOMWLGI-E-WREG-LOG-ANTES.
                   20 DCOMWLGI-E-WREG-LOG-ANTES-LEN  PIC S9(004) COMP.
                   20 DCOMWLGI-E-WREG-LOG-ANTES-TEXT PIC  X(1500).
                15 DCOMWLGI-E-WREG-LOG-APOS.
                   20 DCOMWLGI-E-WREG-LOG-APOS-LEN   PIC S9(004) COMP.
                   20 DCOMWLGI-E-WREG-LOG-APOS-TEXT  PIC  X(1500).
                15 DCOMWLGI-E-CTRANS-PROG-ATULZ      PIC  X(008).
                15 DCOMWLGI-E-CINDCD-MODLD-PROCM     PIC  X(001).
      *
             10 DCOMWLGI-BLOCO-SAIDA.
                15 DCOMWLGI-S-RESERVA                PIC  X(100).
