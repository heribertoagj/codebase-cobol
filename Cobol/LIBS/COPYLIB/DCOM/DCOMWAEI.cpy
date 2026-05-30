      *================================================================*
      * DESCRICAO  : LISTA DETALHADA DE PRODUTOS/SUBPRODUTOS APTOS A   *
      *              OPERAR NO CANAL DO TFWEB                          *
      * COPYBOOK   : DCOMWAEI - BOOK DE ACESSO AO PROGRAMA COORDENADOR *
      * COORDENADOR: DCOM3AEL - LISTA DETALHADA DE PRODUTOS/SUBPRODUTOS*
      * DATA       : 06/11/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *================================================================*
      *
          05 DCOMWAEI-HEADER.
             10 DCOMWAEI-COD-LAYOUT  PIC X(008)      VALUE 'DCOMWAEI'.
             10 DCOMWAEI-TAM-LAYOUT  PIC 9(005)      VALUE 22000.
      *
          05 DCOMWAEI-REGISTRO.
             10 DCOMWAEI-BLOCO-ENTRADA.
                15 DCOMWAEI-E-FUNCAO                 PIC  9(002).
                15 DCOMWAEI-E-CPRODT-SERVC-OPER      PIC  9(008).
                15 DCOMWAEI-E-CPRODT                 PIC  9(003).
                15 DCOMWAEI-E-CSPROD-DESC-COML       PIC  9(003).
                15 DCOMWAEI-E-CTPO-DESC-COML         PIC  9(003).
                15 DCOMWAEI-E-RESERVA                PIC  X(068).
      *
             10 DCOMWAEI-BLOCO-PAGINACAO.
                15 DCOMWAEI-INDICADOR-PAGINACAO      PIC  X(001).
                   88 DCOMWAEI-P-INICIAL             VALUE 'I'.
                   88 DCOMWAEI-P-PRIMEIRA            VALUE 'P'.
                   88 DCOMWAEI-P-SEGUINTE            VALUE 'S'.
                   88 DCOMWAEI-P-ANTERIOR            VALUE 'A'.
                   88 DCOMWAEI-P-ULTIMA              VALUE 'U'.
                15 DCOMWAEI-CHAVE-INICIO.
                   20 DCOMWAEI-I-CPRODT              PIC  9(003).
                   20 DCOMWAEI-I-CSPROD-DESC-COML    PIC  9(003).
                   20 DCOMWAEI-I-RESERVA             PIC  X(050).
                15 DCOMWAEI-CHAVE-FINAL.
                   20 DCOMWAEI-F-CPRODT              PIC  9(003).
                   20 DCOMWAEI-F-CSPROD-DESC-COML    PIC  9(003).
                   20 DCOMWAEI-F-RESERVA             PIC  X(050).
      *
             10 DCOMWAEI-BLOCO-SAIDA.
      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE PRODUTO DO SISTEMA DCOM                  <--
      *-->  -->    CORRESPONDE A TABELA DCOMB047 - TPRODT_VALID_DESC <--
      *-->      ----------------------------------------------------
                15 DCOMWAEI-S-DCOMB047.
                   20 DCOMWAEI-S-B047-CPRODT         PIC  9(003).
                   20 DCOMWAEI-S-B047-IPRODT         PIC  X(060).
                   20 DCOMWAEI-S-B047-IABREV-PRODT   PIC  X(012).
                   20 DCOMWAEI-S-B047-CSGL-PRODT     PIC  X(006).
                   20 DCOMWAEI-S-B047-CIDTFD-TX-MIN  PIC  9(005).
                   20 DCOMWAEI-S-B047-CIDTFD-TX-MAX  PIC  9(005).
                   20 DCOMWAEI-S-B047-CIDTFD-TX-CUS  PIC  9(005).
                   20 DCOMWAEI-S-B047-RESERVA        PIC  X(300).
      *
      *-->      ----------------------------------------------------
      *-->  -->    DADOS DE SUBPRODUTO DO SISTEMA DCOM - MAX 50 OCOR <--
      *-->  -->    CORRESPONDE A TABELA DCOMB091 - TSPROD_DESC_COML  <--
      *-->      ----------------------------------------------------
                15 DCOMWAEI-S-QTOT-DCOMB091          PIC  9(009).
                15 DCOMWAEI-S-QTDE-DCOMB091          PIC  9(002).
                15 DCOMWAEI-S-DCOMB091               OCCURS 030 TIMES.
                   20 DCOMWAEI-S-B091-CPRODT         PIC  9(003).
                   20 DCOMWAEI-S-B091-CSPROD-DESC    PIC  9(003).
                   20 DCOMWAEI-S-B091-DINIC-VGCIA    PIC  X(010).
                   20 DCOMWAEI-S-B091-DFIM-VGCIA     PIC  X(010).
                   20 DCOMWAEI-S-B091-CTPO-DESC      PIC  9(003).
                   20 DCOMWAEI-S-B083-ITPO-DESC      PIC  X(030).
                   20 DCOMWAEI-S-B083-IRSUMO-TPO     PIC  X(010).
                   20 DCOMWAEI-S-B083-CSGL-TPO       PIC  X(006).
                   20 DCOMWAEI-S-B091-ISPROD-DESC    PIC  X(040).
                   20 DCOMWAEI-S-B091-IRSUMO-SPROD   PIC  X(010).
                   20 DCOMWAEI-S-B091-CSGL-SPROD     PIC  X(006).
                   20 DCOMWAEI-S-B091-CSIT-DESC      PIC  9(003).
                   20 DCOMWAEI-S-B057-ISIT-DESC      PIC  X(030).
                   20 DCOMWAEI-S-B057-IRSUMO-SIT     PIC  X(015).
                   20 DCOMWAEI-S-B057-CSGL-SIT       PIC  X(006).
                   20 DCOMWAEI-S-B091-HSIT-DESC      PIC  X(026).
                   20 DCOMWAEI-S-B091-CINDCD-VALDC   PIC  X(001).
                   20 DCOMWAEI-S-B091-DVALDC-ROTRO   PIC  X(010).
                   20 DCOMWAEI-S-B091-CFUNC-VALDC    PIC  9(009).
                   20 DCOMWAEI-S-B091-RSPROD-DESC    PIC  X(240).
                   20 DCOMWAEI-S-B091-CPRODT-BASE    PIC  9(003).
                   20 DCOMWAEI-S-B091-CSPROD-BASE    PIC  9(003).
                   20 DCOMWAEI-S-B091-RJUSTF-SIT     PIC  X(140).
                   20 DCOMWAEI-S-B091-CPRODT-SERVC   PIC  9(008).
                   20 DCOMWAEI-S-B091-CCOOBC-PRODT   PIC  X(001).
      *
      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWAEI-S-RESERVA                PIC  X(2600).
      *
