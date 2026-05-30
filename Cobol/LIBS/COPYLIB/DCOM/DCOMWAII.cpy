      *================================================================*
      * DESCRICAO  : CONSULTAR DADOS DO SUBPRODUTO DCOM APTO A OPERAR  *
      *              NO CANAL TFWEB - PARTE 1                          *
      *          ==> PROCESSO DE CARGA NA COLLECTION MONGODB - PARTE 1 *
      * COPYBOOK   : DCOMWAII - BOOK DE ACESSO AO PROGRAMA FUNCIONAL   *
      * COORDENADOR: DCOM1AIC - CONSULTA DADOS DE PRODUTO/SUBPRODUTO   *
      * FLUXO      : DCOMIABH                                          *
      * DATA       : 28/11/2023                                        *
      * AUTOR      : EDGARD ALMEIDA                                    *
      * COMPONENTE : DCOM - DESCONTO COMERCIAL                         *
      *----------------------------------------------------------------*
      ************************* BLOCO DE ENTRADA ***********************
      **                                                              **
      * DCOMWAII-E-CPRODT     = CODIGO DO PRODUTO DCOM                 *
      * DCOMWAII-E-CSPROD-DESC-COML                                    *
      *                       = CODIGO DO SUBPRODUTO DCOM              *
      * DCOMWAII-E-CTPO-DESC-COML                                      *
      *                       = CODIGO DO TIPO DE DESCONTO             *
      **                                                              **
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      *----------------------------------------------------------------*
      * XX/XX/XXXX XXXXXXXXXXXXXXXXX XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX  ***
      *================================================================*
      *
          05 DCOMWAII-E-HEADER.
             10 DCOMWAII-E-COD-LAYOUT  PIC X(008) VALUE 'DCOMWAII'.
             10 DCOMWAII-E-TAM-LAYOUT  PIC 9(005) VALUE 10144.
      *
          05 DCOMWAII-REGISTRO.
             10 DCOMWAII-BLOCO-ENTRADA.
                15 DCOMWAII-E-CPRODT               PIC  9(003).
                15 DCOMWAII-E-CSPROD-DESC-COML     PIC  9(003).
                15 DCOMWAII-E-RESERVA              PIC  X(094).
      *
             10 DCOMWAII-BLOCO-SAIDA.
      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO SUBPRODUTO REGRA OPERACIONAL - MAX 002   <--
      *-->  -->    CORRESPONDE A TABELA DCOMB067 - TSPROD_REGRA_CONTR<--
      *-->      -----------------------------------------------------
                15 DCOMWAII-QTDE-DCOMB067            PIC  9(002).
                15 DCOMWAII-DCOMB067                 OCCURS 002 TIMES.
                   20 DCOMWAII-B067-CPRODT           PIC  9(003).
                   20 DCOMWAII-B067-CSPROD-DESC-COML PIC  9(003).
                   20 DCOMWAII-B067-HINIC-REGRA-CONT PIC  X(026).
                   20 DCOMWAII-B067-HFIM-REGRA-CONTR PIC  X(026).
                   20 DCOMWAII-B067-CINDCD-TOMAD-OP  PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-PSSOA-TOM PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-CONTR-LIM PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-CONVE-CLI PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-CONVE-GRA PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-BAIXA-P   PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-BAIXA-RTE PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-BAIXA-ANT PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-BAIXA-GST PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-ADTTO-P   PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-PRROG-P   PIC  X(001).
                   20 DCOMWAII-B067-VTARIF-PRROG-P   PIC  9(015)V9(002).
                   20 DCOMWAII-B067-CTARIF-PRROG-P   PIC  9(005).
                   20 DCOMWAII-B067-CRESP-LIM-CREDT  PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-CONS-CRE  PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-ANLSE-CRE PIC  X(001).
                   20 DCOMWAII-B067-HULT-ATULZ       PIC  X(026).
                   20 DCOMWAII-B067-CINDCD-DISPN-AG  PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-DISPN-NE  PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-DISPN-TLB PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-DISPN-IB  PIC  X(001).
                   20 DCOMWAII-B067-CINDCD-ADTTO-TX  PIC  X(001).
                   20 DCOMWAII-B067-QVCTO-PCELA-ADT  PIC  9(003).
                   20 DCOMWAII-B067-CINDCD-ADTTO-OP  PIC  X(001).
                   20 DCOMWAII-B067-PTX-OPCAO-ACRES  PIC  9(009).
                   20 DCOMWAII-B067-PTX-OPCAO-REDC   PIC  9(009).
                   20 FILLER                         PIC  X(206).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO SUBPRODUTO GARANTIAS DE CREDITO - MAX 02 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB062 - TSPROD_GARNT_CREDT<--
      *-->      -----------------------------------------------------
                15 DCOMWAII-QTDE-DCOMB062            PIC  9(002).
                15 DCOMWAII-DCOMB062                 OCCURS 002 TIMES.
                   20 DCOMWAII-B062-CTPO-ISENC-DESC  PIC  9(003).
                   20 DCOMWAII-B062-CPRODT           PIC  9(003).
                   20 DCOMWAII-B062-CSPROD-DESC-COM  PIC  9(003).
                   20 DCOMWAII-B062-DINIC-VGCIA-ISE  PIC  X(010).
                   20 DCOMWAII-B062-DFIM-VGCIA-ISEN  PIC  X(010).
                   20 DCOMWAII-B062-CINDCD-REST-VLR  PIC  X(001).
                   20 DCOMWAII-B062-VREST-MAX-OPER   PIC  9(015)V9(002).
                   20 DCOMWAII-B062-HULT-ATULZ       PIC  X(026).
                   20 DCOMWAII-B085-ITPO-ISENC-DESC  PIC  X(030).
                   20 DCOMWAII-B085-IRSUMO-TPO-ISENC PIC  X(015).
                   20 FILLER                         PIC  X(235).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO ROTEIRO ELEMENTO DA OPERACAO    - MAX 10 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB066 - TSPROD_MEIO_PGTO
      *-->      -----------------------------------------------------
                15 DCOMWAII-QTDE-DCOMB066            PIC  9(002).
                15 DCOMWAII-DCOMB066                 OCCURS 010 TIMES.
                   20 DCOMWAII-B066-CPRODT           PIC  9(003).
                   20 DCOMWAII-B066-CSPROD-DESC-COML PIC  9(003).
                   20 DCOMWAII-B066-CINDCD-VCTO-PCEL PIC  X(001).
                   20 DCOMWAII-B066-CMEIO-PGTO-DESC  PIC  9(003).
                   20 DCOMWAII-B066-HINIC-MEIO-PGTO  PIC  X(026).
                   20 DCOMWAII-B066-CINDCD-RESP-PGTO PIC  X(001).
                   20 DCOMWAII-B066-TDEB-PCELA-VENCD PIC  9(003).
                   20 DCOMWAII-B066-CINDCD-PRZ-PCELA PIC  X(002).
                   20 DCOMWAII-B066-HFIM-MEIO-PGTO   PIC  X(026).
                   20 DCOMWAII-B066-HULT-ATULZ       PIC  X(026).
                   20 DCOMWAII-B066-CINDCD-PGTO-OPCA PIC  X(001).
                   20 DCOMWAII-B066-CINDCD-OPCAO-BLE PIC  X(001).
                   20 DCOMWAII-B090-IMEIO-PGTO-DESC  PIC  X(040).
                   20 DCOMWAII-B090-IRSUMO-MEIO-PGTO PIC  X(015).
                   20 DCOMWAII-B090-CSGL-MEIO-PGTO   PIC  X(006).
                   20 FILLER                         PIC  X(196).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO ROTEIRO ELEMENTO DA OPERACAO    - MAX 08 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB065 - TSPROD_MEIO_PGTO
      *-->      -----------------------------------------------------
                15 DCOMWAII-QTDE-DCOMB065            PIC  9(002).
                15 DCOMWAII-DCOMB065                 OCCURS 008 TIMES.
                   20 DCOMWAII-B065-CPRODT           PIC  9(003).
                   20 DCOMWAII-B065-CSPROD-DESC-COML PIC  9(003).
                   20 DCOMWAII-B065-CMEIO-LIBRC-DESC PIC  9(003).
                   20 DCOMWAII-B065-HINIC-MEIO-LIBRC PIC  X(026).
                   20 DCOMWAII-B065-HFIM-MEIO-LIBRC  PIC  X(026).
                   20 DCOMWAII-B065-HULT-ATULZ       PIC  X(026).
                   20 DCOMWAII-B034-IMEIO-LIBRC-DES  PIC  X(025).
                   20 DCOMWAII-B034-IRSUMO-MEIO-LIB  PIC  X(015).
                   20 DCOMWAII-B034-CSGL-MEIO-LIBRC  PIC  X(006).
                   20 FILLER                         PIC  X(220).

      *-->      -----------------------------------------------------
      *-->  -->    DADOS DO ROTEIRO ELEMENTO DA OPERACAO    - MAX 08 <--
      *-->  -->    CORRESPONDE A TABELA DCOMB064 - TSPROD_MEIO_PGTO
      *-->      -----------------------------------------------------
                15 DCOMWAII-QTDE-DCOMB064            PIC  9(002).
                15 DCOMWAII-DCOMB064                 OCCURS 005 TIMES.
                   20 DCOMWAII-B064-CPRODT           PIC  9(003).
                   20 DCOMWAII-B064-CSPROD-DESC-COML PIC  9(003).
                   20 DCOMWAII-B064-CMEIO-ENTRD-DESC PIC  9(003).
                   20 DCOMWAII-B064-HINIC-MEIO-ENTRD PIC  X(026).
                   20 DCOMWAII-B064-CINDCD-MODLD-DE  PIC  X(001).
                   20 DCOMWAII-B064-HFIM-MEIO-ENTRD  PIC  X(026).
                   20 DCOMWAII-B064-CSIT-DESC-COML   PIC  9(003).
                   20 DCOMWAII-B064-HSIT-DESC-COML   PIC  X(026).
                   20 DCOMWAII-B064-CINDCD-FORML-OP  PIC  X(001).
                   20 DCOMWAII-B064-CINDCD-LIBRC-OP  PIC  X(001).
                   20 DCOMWAII-B064-CINDCD-LIBRC-ANT PIC  X(001).
                   20 DCOMWAII-B064-PMAX-LIBRC-ANTCP PIC  9(003)V9(003).
                   20 DCOMWAII-B064-TMIN-VCTO-TITLO  PIC  9(005).
                   20 DCOMWAII-B064-TMAX-VCTO-TITLO  PIC  9(005).
                   20 DCOMWAII-B064-CINDCD-PRZ-VCTO  PIC  X(002).
                   20 DCOMWAII-B064-TMAX-ACLTO-TITLO PIC  9(003).
                   20 DCOMWAII-B064-CINDCD-PRZ-ACLTO PIC  X(002).
                   20 DCOMWAII-B064-VMAX-DIVRG-TITLO PIC  9(015)V9(002).
                   20 DCOMWAII-B064-CINDCD-OBRIG-GAR PIC  X(001).
                   20 DCOMWAII-B064-CINDCD-OBRIG-PRO PIC  X(001).
                   20 DCOMWAII-B064-CCART-NORML-DESC PIC  X(005).
                   20 DCOMWAII-B064-CCART-VENCD-DESC PIC  X(005).
                   20 DCOMWAII-B064-CCART-DESC-PRODT PIC  X(005).
                   20 DCOMWAII-B064-CINDCD-TARIF-TR  PIC  X(001).
                   20 DCOMWAII-B064-VTARIF-TRNSF-CA  PIC  9(015)V9(002).
                   20 DCOMWAII-B064-CTARIF-TRNSF-CA  PIC  9(005).
                   20 DCOMWAII-B064-CCOBR-TAC-OPER   PIC  9(003).
                   20 DCOMWAII-B064-VTAC-OPER-DESC   PIC  9(015)V9(002).
                   20 DCOMWAII-B064-CTAC-OPER-DESC   PIC  9(005).
                   20 DCOMWAII-B064-CTAC-PERC-OPER   PIC  9(005).
                   20 DCOMWAII-B064-CTAC-MIN-OPER    PIC  9(005).
                   20 DCOMWAII-B064-CTAC-MAX-OPER    PIC  9(005).
                   20 DCOMWAII-B064-CCOBR-TAC-CONTR  PIC  9(003).
                   20 DCOMWAII-B064-VTAC-OPER-EFETU  PIC  9(015)V9(002).
                   20 DCOMWAII-B064-CTAC-OPER-EFETU  PIC  9(005).
                   20 DCOMWAII-B064-CTAC-PERC-CONTR  PIC  9(005).
                   20 DCOMWAII-B064-CTAC-MIN-CONTR   PIC  9(005).
                   20 DCOMWAII-B064-CTAC-MAX-CONTR   PIC  9(005).
                   20 DCOMWAII-B064-HULT-ATULZ       PIC  X(026).
                   20 DCOMWAII-B064-WCTRL-FLUXO-DIGT PIC  X(015).
                   20 DCOMWAII-B064-CINDCD-TARIF-REG PIC  X(001).
                   20 DCOMWAII-B064-CINDCD-PRZ-MAX   PIC  X(002).
                   20 DCOMWAII-B033-IMEIO-ENTRD-DESC PIC  X(025).
                   20 DCOMWAII-B033-IRSUMO-MEIO-ENTR PIC  X(015).
                   20 DCOMWAII-B033-CSGL-MEIO-ENTRD  PIC  X(006).

      *-->      ----------------------------------------------------
      *-->  -->    AREA RESERVA PARA EXPANSAO DE DADOS               <--
      *-->      ----------------------------------------------------
                15 DCOMWAII-S-RESERVA                PIC  X(0535).

