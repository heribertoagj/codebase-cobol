      *================================================================*
      * DESCRICAO  :-CONSULTAR DADOS DO SUBPRODUTO DCOM APTO A OPERAR  *
      *              NO CANEL TFWEB                                    *
      *             -PROCESSO DE CARGA NA COLLECTION MONGODB - PARTE 2 *
      * COPYBOOK   : DCOMWALE - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1ALC - CONSULTA SUBPRODUTO - < PARTE 2 >      *
      * FLUXO      : DCOMIABJ                                          *
      * DATA       : 22/11/2023                                        *
      * AUTOR      : LUCIANDRA SILVEIRA - WIPRO                        *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWALS-HEADER.
             10 DCOMWALS-COD-LAYOUT    PIC X(008)   VALUE 'DCOMWALS'.
             10 DCOMWALS-TAM-LAYOUT    PIC 9(005)   VALUE 10000.

             10 DCOMWALS-BLOCO-SAIDA.
      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO SUBPRODUTO REGRA OPERACIONAL - MAX 001   <--
      *-->  -->    CORRESPONDE A TABELA DCOMB068 - TSPROD_REGRA_OPER <--
      *-->      -----------------------------------------------------
                15 DCOMWALS-QTDE-DCOMB068          PIC  9(002).
                15 DCOMWALS-DCOMB068               OCCURS 001 TIMES.
                   20 DCOMWALS-B068-HINIC-REGRA    PIC  X(026).
                   20 DCOMWALS-B068-HFIM-REGRA     PIC  X(026).
                   20 DCOMWALS-B068-CALC-JURO      PIC  X(001).
                   20 DCOMWALS-B068-PGDOR-JURO     PIC  X(001).
                   20 DCOMWALS-B068-TX-REDE        PIC  X(001).
                   20 DCOMWALS-B068-INCID-IOF      PIC  X(001).
                   20 DCOMWALS-B068-DESC-SACDO     PIC  X(001).
                   20 DCOMWALS-B068-NEGOC-DIFER    PIC  X(001).
                   20 DCOMWALS-B068-VMIN-OPER-DESC PIC  9(015)V9(002).
                   20 DCOMWALS-B068-VMAX-OPER-DESC PIC  9(015)V9(002).
                   20 DCOMWALS-B068-CREGRA-MORA-LG PIC  9(003).
                   20 DCOMWALS-B052-IREGRA-MORA-LG PIC  X(040).
                   20 DCOMWALS-B068-CREGRA-GERC-CS PIC  9(003).
                   20 DCOMWALS-B051-IREGRA-GERC-CS PIC  X(040).
                   20 DCOMWALS-B068-CREGRA-GERC-MR PIC  9(003).
                   20 DCOMWALS-B051-IREGRA-GERC-MR PIC  X(040).
                   20 DCOMWALS-B068-CREGRA-GERC-CR PIC  9(003).
                   20 DCOMWALS-B051-IREGRA-GERC-CR PIC  X(040).
                   20 DCOMWALS-B068-HULT-ATULZ     PIC  X(026).
                   20 DCOMWALS-B068-QDIA-PROTE-TTL PIC  9(003).
                   20 DCOMWALS-B068-CINDCD-DIA-PRT PIC  X(002).
                   20 DCOMWALS-B068-HINIC-REGBACEN PIC  X(008).
                   20 DCOMWALS-B068-HFIM-REGBACEN  PIC  X(008).
                   20 DCOMWALS-B068-CREG-OPERBACEN PIC  X(001).
                   20 DCOMWALS-B068-CINDCD-AMBTL   PIC  X(001).
                   20 DCOMWALS-B068-QDIA-CALC-JURO PIC  9(003).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO SUBPRODUTO GARANTIAS DE CREDITO - MAX 05 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB0F0 - TSPROD_GARNT_CREDT<--
      *-->      -----------------------------------------------------
                15 DCOMWALS-QTDE-DCOMB0F0          PIC  9(002).
                15 DCOMWALS-DCOMB0F0               OCCURS 005 TIMES.
                   20 DCOMWALS-B0F0-CMEIO-ENTRD    PIC  9(003).
                   20 DCOMWALS-B0F0-CTPO-GARNT     PIC  9(003).
                   20 DCOMWALS-B0F0-ITPO-GARNT     PIC  X(060).
                   20 DCOMWALS-B0F0-HINIC-VGCIA    PIC  X(026).
                   20 DCOMWALS-B0F0-HFIM-VGCIA     PIC  X(026).
                   20 DCOMWALS-B0F0-HULT-ATULZ     PIC  X(026).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO ROTEIRO ELEMENTO DA OPERACAO    - MAX 10 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB054 - TROTRO_ELMTO_DESC
      *-->      -----------------------------------------------------
                15 DCOMWALS-QTDE-DCOMB054          PIC  9(002).
                15 DCOMWALS-DCOMB054               OCCURS 050 TIMES.
                   20 DCOMWALS-B054-CTPO-AGPTO-RT  PIC  9(003).
                   20 DCOMWALS-B078-ITPO-AGPTO-RT  PIC  X(025).
                   20 DCOMWALS-B054-CINDCD-OBRIG   PIC  X(001).
                   20 DCOMWALS-B054-CTRANS-PROG    PIC  X(008).
                   20 DCOMWALS-B054-HULT-ATULZ     PIC  X(026).
                   20 DCOMWALS-B054-CPRODT         PIC  9(003).
                   20 DCOMWALS-B054-CTPO-DESC-COML PIC  9(003).
                   20 DCOMWALS-B054-CMEIO-ENTRD    PIC  9(003).
                   20 DCOMWALS-B054-CELMTO-DESC    PIC  9(003).
                   20 DCOMWALS-B023-IELMTO-DESC    PIC  X(030).

      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWALS-RESERVA                PIC  X(3695).
