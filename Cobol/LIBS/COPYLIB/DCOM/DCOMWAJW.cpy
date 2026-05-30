      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE CONTRATO LIMITE - PAGINACAO    *
      * COPYBOOK   : DCOMWAJW - BOOK DE PAGINACAO PROGRAMA COORDENADOR *
      * COORDENADOR: DCOM3AJL - LISTA DETALHADA DE CONTRATO LIMITE     *
      * DATA       : 07/08/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    *
      *================================================================*
      *
          05 DCOMWAJW-HEADER.
             10 DCOMWAJW-COD-LAYOUT  PIC X(008)      VALUE 'DCOMWAJW'.
             10 DCOMWAJW-TAM-LAYOUT  PIC 9(005)      VALUE 00247.
      *
          05 DCOMWAJW-REGISTRO.
             10 DCOMWAJW-COD-RETORNO                 PIC  9(002).
             10 DCOMWAJW-BLOCO-ENTRADA.
                15 DCOMWAJW-E-FUNCAO                 PIC  9(002).
                15 DCOMWAJW-E-CPRODT-SERVC-OPER      PIC  9(008).
                15 DCOMWAJW-E-CPRODT                 PIC  9(003).
                15 DCOMWAJW-E-CSPROD-DESC-COML       PIC  9(003).
                15 DCOMWAJW-E-CBCO                   PIC  9(003).
                15 DCOMWAJW-E-CAG-BCRIA              PIC  9(005).
                15 DCOMWAJW-E-CCTA-BCRIA-CLI         PIC  9(013).
                15 DCOMWAJW-E-CCNPJ-CPF              PIC  9(009).
                15 DCOMWAJW-E-CFLIAL-CNPJ            PIC  9(004).
                15 DCOMWAJW-E-CCTRL-CNPJ-CPF         PIC  9(002).
                15 DCOMWAJW-E-CCONTR-LIM-DESC        PIC  9(009).
                15 DCOMWAJW-E-CVRSAO-CONTR-LIM       PIC  9(003).
                15 DCOMWAJW-E-DANO-OPER-DESC         PIC  9(004).
                15 DCOMWAJW-E-NSEQ-OPER-DESC         PIC  9(009).
                15 DCOMWAJW-E-RESERVA                PIC  X(010).
      *
             10 DCOMWAJW-BLOCO-PAGINACAO.
                15 DCOMWAJW-INDICADOR-PAGINACAO      PIC  X(001).
                   88 DCOMWAJW-P-INICIAL             VALUE 'I'.
                   88 DCOMWAJW-P-PRIMEIRA            VALUE 'P'.
                   88 DCOMWAJW-P-SEGUINTE            VALUE 'S'.
                   88 DCOMWAJW-P-ANTERIOR            VALUE 'A'.
                   88 DCOMWAJW-P-ULTIMA              VALUE 'U'.
               15 DCOMWAJW-CHAVE-INICIO.
                  20 DCOMWAJW-I-CCONTR-LIM-DESC      PIC  9(009).
                  20 DCOMWAJW-I-CVRSAO-CONTR-LIM     PIC  9(003).
                  20 DCOMWAJW-I-DINIC-VGCIA-CONTR    PIC  X(010).
                  20 DCOMWAJW-I-RESERVA              PIC  X(050).
               15 DCOMWAJW-CHAVE-FINAL.
                  20 DCOMWAJW-F-CCONTR-LIM-DESC      PIC  9(009).
                  20 DCOMWAJW-F-CVRSAO-CONTR-LIM     PIC  9(003).
                  20 DCOMWAJW-F-DINIC-VGCIA-CONTR    PIC  X(010).
                  20 DCOMWAJW-F-RESERVA              PIC  X(050).
      *
