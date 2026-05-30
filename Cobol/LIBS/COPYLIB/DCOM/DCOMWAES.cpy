      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE PRODUTOS/SUBPRODUTOS APTOS A   *
      *              OPERAR NO CANAL TFWEB - LAYOUT DE SAIDA           *
      * COPYBOOK   : DCOMWAES - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AEL - LISTA DETALHADA DE PRODUTO/SUBPRODUTO  *
      *                         APTOS A OPERAR NO CANAL TFWEB          *
      * FLUXO      : DDCOMIABG                                         *
      * DATA       : 03/11/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWAES-HEADER.
             10 DCOMWAES-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWAES'.
             10 DCOMWAES-TAM-LAYOUT    PIC 9(005)   VALUE 22000.

             10 DCOMWAES-BLOCO-SAIDA.
      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE PRODUTO DO SISTEMA DCOM                  <--
      *-->  -->    CORRESPONDE A TABELA DCOMB047 - TPRODT_VALID_DESC <--
      *-->      ----------------------------------------------------
                15 DCOMWAES-DCOMB047.
                   20 DCOMWAES-B047-CPRODT         PIC  9(003).
                   20 DCOMWAES-B047-IPRODT         PIC  X(060).
                   20 DCOMWAES-B047-IABREV-PRODT   PIC  X(012).
                   20 DCOMWAES-B047-CSGL-PRODT     PIC  X(006).
                   20 DCOMWAES-B047-CIDTFD-TX-MIN  PIC  9(005).
                   20 DCOMWAES-B047-CIDTFD-TX-MAX  PIC  9(005).
                   20 DCOMWAES-B047-CIDTFD-TX-CUS  PIC  9(005).
                   20 DCOMWAES-B047-RESERVA        PIC  X(300).

      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE SUBPRODUTO DO SISTEMA DCOM - MAX 30 OCOR <--
      *-->  -->    CORRESPONDE A TABELA DCOMB091 - TSPROD_DESC_COML  <--
      *-->      ----------------------------------------------------
                15 DCOMWAES-QTOT-DCOMB091          PIC  9(009).
                15 DCOMWAES-QTDE-DCOMB091          PIC  9(002).
                15 DCOMWAES-DCOMB091               OCCURS 030 TIMES.
                   20 DCOMWAES-B091-CPRODT         PIC  9(003).
                   20 DCOMWAES-B091-CSPROD-DESC    PIC  9(003).
                   20 DCOMWAES-B091-DINIC-VGCIA    PIC  X(010).
                   20 DCOMWAES-B091-DFIM-VGCIA     PIC  X(010).
                   20 DCOMWAES-B091-CTPO-DESC      PIC  9(003).
                   20 DCOMWAES-B083-ITPO-DESC      PIC  X(030).
                   20 DCOMWAES-B083-IRSUMO-TPO     PIC  X(010).
                   20 DCOMWAES-B083-CSGL-TPO       PIC  X(006).
                   20 DCOMWAES-B091-ISPROD-DESC    PIC  X(040).
                   20 DCOMWAES-B091-IRSUMO-SPROD   PIC  X(010).
                   20 DCOMWAES-B091-CSGL-SPROD     PIC  X(006).
                   20 DCOMWAES-B091-CSIT-DESC      PIC  9(003).
                   20 DCOMWAES-B057-ISIT-DESC      PIC  X(030).
                   20 DCOMWAES-B057-IRSUMO-SIT     PIC  X(015).
                   20 DCOMWAES-B057-CSGL-SIT       PIC  X(006).
                   20 DCOMWAES-B091-HSIT-DESC      PIC  X(026).
                   20 DCOMWAES-B091-CINDCD-VALDC   PIC  X(001).
                   20 DCOMWAES-B091-DVALDC-ROTRO   PIC  X(010).
                   20 DCOMWAES-B091-CFUNC-VALDC    PIC  9(009).
                   20 DCOMWAES-B091-RSPROD-DESC    PIC  X(240).
                   20 DCOMWAES-B091-CPRODT-BASE    PIC  9(003).
                   20 DCOMWAES-B091-CSPROD-BASE    PIC  9(003).
                   20 DCOMWAES-B091-RJUSTF-SIT     PIC  X(140).
                   20 DCOMWAES-B091-CPRODT-SERVC   PIC  9(008).
                   20 DCOMWAES-B091-CCOOBC-PRODT   PIC  X(001).

      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWAES-RESERVA                PIC  X(2800).
