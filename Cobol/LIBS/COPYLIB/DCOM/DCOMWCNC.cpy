      ******************************************************************
      * DESCRICAO..: AREA DE ACESSO DO PROGRAMA DCOM3PTC: PARA OBTER   *
      *      - OBTER INFORMACOES DO ULTIMO PROTOCOLO CONFORME O TIPO   *
      *              (BASEADO NO BOOK I#DCOMZD)                        *
      *      - INCLUIR OU ALTERAR PROTOCOLO CONFORME O TIPO            *
      * AUTOR......: LUCIANDRA SILVEIRA                                *
      * COMPONENTE.: DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      ******************************************************************
          05 DCOMWCNC-HEADER.
             10 DCOMWCNC-COD-LAYOUT    PIC X(008)  VALUE 'DCOMWCNC'.
             10 DCOMWCNC-TAM-LAYOUT    PIC 9(005)  VALUE 00113.
      *
          05 DCOMWCNC-REGISTRO.
             10 DCOMWCNC-BLOCO-ENTRADA.
                15 DCOMWCNC-E-CCANAL               PIC  9(003).
                15 DCOMWCNC-E-ICANAL               PIC  X(008).
                15 DCOMWCNC-E-CAUTEN-SEGRC.
                   20 DCOMWCNC-E-CAUTEN-SEGRC-N    PIC  9(009).
                15 DCOMWCNC-E-RESERVA              PIC  X(030).
      *
             10 DCOMWCNC-BLOCO-SAIDA.
                15 DCOMWCNC-S-CCANAL               PIC  9(003).
                15 DCOMWCNC-S-ICANAL               PIC  X(008).
                15 DCOMWCNC-S-CAUTEN-SEGRC.
                   20 DCOMWCNC-S-CAUTEN-SEGRC-N    PIC  9(009).
                15 DCOMWCNC-S-RESERVA              PIC  X(030).
