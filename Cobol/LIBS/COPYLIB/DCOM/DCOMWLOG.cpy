      ******************************************************************
      * DESCRICAO  : AREA DE ACESSO AO MODULO DE INCLUSAO E MANUTENCAO *
      *              DE LOGS DO SISTEMA DCOM                           *
      * COPYBOOK   : DCOMWLOG - BOOK DE ACESSO AO PROGRAMA DCOM3LOG    *
      * FUNCIONAL  : DCOM3LOG - INCLUSAO E MANUTENCAO DE LOGS DO DCOM  *
      * AUTOR      : LUCIANDRA SILVEIRA                                *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
      *
          05 DCOMWLOG-HEADER.
             10 DCOMWLOG-COD-LAYOUT    PIC X(008) VALUE 'DCOMWLOG'.
             10 DCOMWLOG-TAM-LAYOUT    PIC 9(005) VALUE 06400.
      *
          05 DCOMWLOG-REGISTRO.
             10 DCOMWLOG-BLOCO-ENTRADA.
                15 DCOMWLOG-E-CELMTO-DESC-COML    PIC  9(003).
                15 DCOMWLOG-E-CEVNTO-LOG-DESC     PIC  9(003).
                15 DCOMWLOG-E-HULT-ATULZ          PIC  X(026).
                15 DCOMWLOG-E-CINDCD-MANUT-REG    PIC  X(001).
                15 DCOMWLOG-E-CCHAVE-ELMTO-DESC   PIC  X(045).
                15 DCOMWLOG-E-WREG-LOG-ANTES.
                   20 DCOMWLOG-E-ANTES-LEN        PIC S9(004) COMP.
                   20 DCOMWLOG-E-ANTES-TEXT       PIC X(1500).
                15 DCOMWLOG-E-WREG-LOG-APOS.
                   20 DCOMWLOG-E-APOS-LEN         PIC S9(004) COMP.
                   20 DCOMWLOG-E-APOS-TEXT        PIC X(1500).
                15 DCOMWLOG-E-CTRANS-PROG-ATULZ   PIC  X(008).
                15 DCOMWLOG-E-CINDCD-MODLD-PROCM  PIC  X(001).
                15 DCOMWLOG-E-CEMPR-INC           PIC  9(005).
                15 DCOMWLOG-E-CDEPDC              PIC  9(005).
                15 DCOMWLOG-E-CFUNC-BDSCO         PIC  9(009).
                15 DCOMWLOG-E-CTERM               PIC  X(008).
                15 DCOMWLOG-E-FILLER              PIC  X(050).
      *
             10 DCOMWLOG-BLOCO-SAIDA.
                15 DCOMWLOG-S-CELMTO-DESC-COML    PIC  9(003).
                15 DCOMWLOG-S-CEVNTO-LOG-DESC     PIC  9(003).
                15 DCOMWLOG-S-HULT-ATULZ          PIC  X(026).
                15 DCOMWLOG-S-CINDCD-MANUT-REG    PIC  X(001).
                15 DCOMWLOG-S-CCHAVE-ELMTO-DESC   PIC  X(045).
                15 DCOMWLOG-S-WREG-LOG-ANTES.
                   20 DCOMWLOG-S-ANTES-LEN        PIC S9(004) COMP.
                   20 DCOMWLOG-S-ANTES-TEXT       PIC X(1500).
                15 DCOMWLOG-S-WREG-LOG-APOS.
                   20 DCOMWLOG-S-APOS-LEN         PIC S9(004) COMP.
                   20 DCOMWLOG-S-APOS-TEXT        PIC X(1500).
                15 DCOMWLOG-S-CTRANS-PROG-ATULZ   PIC  X(008).
                15 DCOMWLOG-S-CINDCD-MODLD-PROCM  PIC  X(001).
                15 DCOMWLOG-S-CEMPR-INC           PIC  9(005).
                15 DCOMWLOG-S-CDEPDC              PIC  9(005).
                15 DCOMWLOG-S-CFUNC-BDSCO         PIC  9(009).
                15 DCOMWLOG-S-CTERM               PIC  X(008).
                15 DCOMWLOG-FILLER                PIC  X(051).
