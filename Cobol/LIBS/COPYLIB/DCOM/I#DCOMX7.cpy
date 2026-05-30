      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMX7 *
      * CRIACAO   : 08/2006                                   *
      * DESCRICAO : CALCULAR DEMONSTRATIVO FINANCEIRO         *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM5333                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-5333.
           03 WRK-ENVIO-5333.
              05 WRK-CINDCD-PSSOA-TOMAD-5333    PIC  X(01).
              05 WRK-CTPO-RAMO-ATVDD-5333       PIC  9(03).
              05 WRK-CPORTE-EMPR-5333           PIC  9(03).
              05 WRK-CNATUZ-JURID-5333          PIC  9(03).
              05 WRK-VOPER-DESC-COML-5333       PIC  S9(15)V9(2).
              05 WRK-VJURO-OPER-DESC-5333       PIC  S9(15)V9(2).
              05 WRK-CCONTR-LIM-DESC-5333       PIC  9(09).
              05 WRK-CVRSAO-CONT-LIM-5333       PIC  9(03).
              05 WRK-CPRODT-5333                PIC  9(03).
              05 WRK-CSPROD-DESC-COML-5333      PIC  9(03).
              05 WRK-CTPO-DESC-COML-5333        PIC  9(03).
              05 WRK-CMEIO-ENTRD-DESC-5333      PIC  9(03).
              05 WRK-DINIC-OPER-DESC-5333       PIC  X(10).
              05 WRK-TMED-PONDE-OPER-5333       PIC  9(03).
              05 WRK-QTITLO-CHEQ-PCELA-5333     PIC  9(09).
              05 WRK-CELMTO-DESC-COML-5333      PIC  9(03).

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************

           03 WRK-RETORNO-5333.
              05 WRK-COD-RET-5333               PIC  9(04).
              05 WRK-MENS-RET-5333              PIC  X(79).
              05 WRK-VTAC-OPER-DESC-5333        PIC  S9(15)V9(2).
              05 WRK-VTARIF-REG-TITLO-5333      PIC  S9(15)V9(2).
              05 WRK-VIOF-OPER-DESC-5333        PIC  S9(15)V9(2).
              05 WRK-VLIQ-OPER-DESC-5333        PIC  S9(15)V9(2).
