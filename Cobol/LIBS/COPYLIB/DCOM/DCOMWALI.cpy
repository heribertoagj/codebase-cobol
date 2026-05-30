      *================================================================*
      * DESCRICAO  : CONSULTAR DADOS DO SUBPRODUTO DCOM APTO A OPERAR  *
      *              NO CANAL TFWEB - PARTE 2                          *
      *          ==> PROCESSO DE CARGA NA COLLECTION MONGODB - PARTE 2 *
      * COPYBOOK   : DCOMWALI - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ALC - CONSULTA DADOS DE PRODUTO/SUBPRODUTO   *
      * FLUXO      : DCOMIABJ                                          *
      * DATA       : 22/11/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWALI-E-CPRODT     = CODIGO DO PRODUTO DCOM                 *
      * DCOMWALI-E-CSPROD-DESC-COML                                    *
      *                       = CODIGO DO SUBPRODUTO DCOM              *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWALI-E-HEADER.
             10 DCOMWALI-E-COD-LAYOUT  PIC X(008) VALUE 'DCOMWALI'.
             10 DCOMWALI-E-TAM-LAYOUT  PIC 9(005) VALUE 10000.
      *
          05 DCOMWALI-REGISTRO.
             10 DCOMWALI-BLOCO-ENTRADA.
                15 DCOMWALI-E-CPRODT               PIC  9(003).
                15 DCOMWALI-E-CSPROD-DESC-COML     PIC  9(003).
                15 DCOMWALI-E-RESERVA              PIC  X(081).
      *
             10 DCOMWALI-BLOCO-SAIDA.
      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO SUBPRODUTO REGRA OPERACIONAL - MAX 001   <--
      *-->  -->    CORRESPONDE A TABELA DCOMB068 - TSPROD_REGRA_OPER <--
      *-->      -----------------------------------------------------
                15 DCOMWALI-S-QTDE-DCOMB068        PIC  9(002).
                15 DCOMWALI-S-DCOMB068             OCCURS 001 TIMES.
                   20 DCOMWALI-S-B068-HINIC-REGRA  PIC  X(026).
                   20 DCOMWALI-S-B068-HFIM-REGRA   PIC  X(026).
                   20 DCOMWALI-S-B068-CALC-JURO    PIC  X(001).
                   20 DCOMWALI-S-B068-PGDOR-JURO   PIC  X(001).
                   20 DCOMWALI-S-B068-TX-REDE      PIC  X(001).
                   20 DCOMWALI-S-B068-INCID-IOF    PIC  X(001).
                   20 DCOMWALI-S-B068-DESC-SACDO   PIC  X(001).
                   20 DCOMWALI-S-B068-NEGOC-DIFER  PIC  X(001).
                   20 DCOMWALI-S-B068-VMIN-OPER    PIC  9(015)V9(002).
                   20 DCOMWALI-S-B068-VMAX-OPER    PIC  9(015)V9(002).
                   20 DCOMWALI-S-B068-CREGRA-MR-LG PIC  9(003).
                   20 DCOMWALI-S-B052-IREGRA-MR-LG PIC  X(040).
                   20 DCOMWALI-S-B068-CREGRA-GR-CS PIC  9(003).
                   20 DCOMWALI-S-B051-IREGRA-GR-CS PIC  X(040).
                   20 DCOMWALI-S-B068-CREGRA-GR-MR PIC  9(003).
                   20 DCOMWALI-S-B051-IREGRA-GR-MR PIC  X(040).
                   20 DCOMWALI-S-B068-CREGRA-GR-CR PIC  9(003).
                   20 DCOMWALI-S-B051-IREGRA-GR-CR PIC  X(040).
                   20 DCOMWALI-S-B068-HULT-ATULZ   PIC  X(026).
                   20 DCOMWALI-S-B068-QDIA-PROTE   PIC  9(003).
                   20 DCOMWALI-S-B068-CINDCD-DIA-P PIC  X(002).
                   20 DCOMWALI-S-B068-HINIC-RBACEN PIC  X(008).
                   20 DCOMWALI-S-B068-HFIM-RBACEN  PIC  X(008).
                   20 DCOMWALI-S-B068-CREG-OPERBAC PIC  X(001).
                   20 DCOMWALI-S-B068-CINDCD-AMBTL PIC  X(001).
                   20 DCOMWALI-S-B068-QDIA-CL-JURO PIC  9(003).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO SUBPRODUTO GARANTIAS DE CREDITO - MAX 05 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB0F0 - TSPROD_GARNT_CREDT<--
      *-->      -----------------------------------------------------
                15 DCOMWALI-S-QTDE-DCOMB0F0        PIC  9(002).
                15 DCOMWALI-S-DCOMB0F0             OCCURS 005 TIMES.
                   20 DCOMWALI-S-B0F0-CMEIO-ENTRD  PIC  9(003).
                   20 DCOMWALI-S-B0F0-CTPO-GARNT   PIC  9(003).
                   20 DCOMWALI-S-B0F0-ITPO-GARNT   PIC  X(060).
                   20 DCOMWALI-S-B0F0-HINIC-VGCIA  PIC  X(026).
                   20 DCOMWALI-S-B0F0-HFIM-VGCIA   PIC  X(026).
                   20 DCOMWALI-S-B0F0-HULT-ATULZ   PIC  X(026).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO ROTEIRO ELEMENTO DA OPERACAO    - MAX 10 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB054 - TROTRO_ELMTO_DESC
      *-->      -----------------------------------------------------
                15 DCOMWALI-S-QTDE-DCOMB054        PIC  9(002).
                15 DCOMWALI-S-DCOMB054             OCCURS 050 TIMES.
                   20 DCOMWALI-S-B054-CTPO-AGPTO   PIC  9(003).
                   20 DCOMWALI-S-B078-ITPO-AGPTO   PIC  X(025).
                   20 DCOMWALI-S-B054-CINDCD-OBRIG PIC  X(001).
                   20 DCOMWALI-S-B054-CTRANS-PROG  PIC  X(008).
                   20 DCOMWALI-S-B054-HULT-ATULZ   PIC  X(026).
                   20 DCOMWALI-S-B054-CPRODT       PIC  9(003).
                   20 DCOMWALI-S-B054-CTPO-DESC    PIC  9(003).
                   20 DCOMWALI-S-B054-CMEIO-ENTRD  PIC  9(003).
                   20 DCOMWALI-S-B054-CELMTO-DESC  PIC  9(003).
                   20 DCOMWALI-S-B023-IELMTO-DESC  PIC  X(030).

      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWALI-S-RESERVA              PIC  X(3608).

