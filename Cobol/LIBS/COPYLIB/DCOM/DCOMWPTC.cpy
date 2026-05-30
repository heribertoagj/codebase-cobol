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
          05 DCOMWPTC-HEADER.
             10 DCOMWPTC-COD-LAYOUT    PIC X(008) VALUE 'DCOMWPTC'.
             10 DCOMWPTC-TAM-LAYOUT    PIC 9(005) VALUE 00200.
      *
          05 DCOMWPTC-REGISTRO.
             10 DCOMWPTC-BLOCO-ENTRADA.
      *-->   -> ACAO PARA PROCESSAMENTO:
      *-->   -> =======================
      *-->      "I" - INCLUIR
      *-->      "A" - ATUALIZAR
      *-->      "C" - CONSULTAR
                15 DCOMWPTC-E-ACAO                PIC  X(001).
      *-->
      *-->      DADOS DO PROTOCOLO
      *-->   -> =======================
                15 DCOMWPTC-E-CBCO                PIC  9(003).
                15 DCOMWPTC-E-CAG-BCRIA           PIC  9(005).
                15 DCOMWPTC-E-CCTA-BCRIA-CLI      PIC  9(013).
                15 DCOMWPTC-E-CPROT-SOLTC-AGNDA   PIC  X(050).
                15 DCOMWPTC-E-CTPO-PROT-AGNDA     PIC  X(001).
                15 DCOMWPTC-E-QTEMPO-VALDD-PROCM  PIC  9(003).
                15 DCOMWPTC-E-CSIT-PROT-AGNDA     PIC  9(001).
                15 DCOMWPTC-E-HINCL-REG           PIC  X(026).
                15 DCOMWPTC-E-NSMULA-OPER-DESC    PIC  9(009).
                15 DCOMWPTC-E-DANO-OPER-DESC      PIC  9(004).
                15 DCOMWPTC-E-NSEQ-OPER-DESC      PIC  9(009).
      *-->
      *-->      DADOS DO CANAL
      *-->   -> =======================
                15 DCOMWPTC-E-CCANAL-DESC         PIC  9(003).
                15 DCOMWPTC-E-QTEMPO-PROCM-CANAL  PIC  9(003).
      *
             10 DCOMWPTC-BLOCO-SAIDA.
                15 DCOMWPTC-S-CBCO                PIC  9(003).
                15 DCOMWPTC-S-CAG-BCRIA           PIC  9(005).
                15 DCOMWPTC-S-CCTA-BCRIA-CLI      PIC  9(013).
                15 DCOMWPTC-S-CPROT-SOLTC-AGNDA   PIC  X(050).
                15 DCOMWPTC-S-QTEMPO-VALDD-PROCM  PIC  9(003).
                15 DCOMWPTC-S-CSIT-PROT-AGNDA     PIC  9(001).
                15 DCOMWPTC-S-HINCL-REG           PIC  X(026).
                15 DCOMWPTC-S-CTPO-CANAL          PIC  9(003).
                15 DCOMWPTC-S-QTEMPO-PROCM-CANAL  PIC  9(003).
      *
             10 DCOMWPTC-RESERVA                  PIC  X(062).
