      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMY1 *
      * CRIACAO   : 09/2006                                   *
      * DESCRICAO : CALCULAR VALOR DO JUROS                   *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM5370                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * ÙREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-5370.
           03 WRK-ENVIO-5370.
            05 WRK-DANO-OPER-DESC-5370        PIC  9(04).
            05 WRK-NSEQ-OPER-DESC-5370        PIC  9(09).
            05 WRK-CPRODT-5370                PIC  9(03).
            05 WRK-CSPROD-DESC-COML-5370      PIC  9(03).
            05 WRK-CTPO-DESC-COML-5370        PIC  9(03).
            05 WRK-DINIC-OPER-DESC-5370       PIC  X(10).
            05 WRK-PTX-JURO-MES-5370          PIC  S9(8)V9(5).
            05 WRK-PTX-JURO-ANO-5370          PIC  S9(8)V9(5).

      *********************************************************
      * ÙREA DE RETORNO                                       *
      *********************************************************

           03 WRK-RETORNO-5370.
              05 WRK-COD-RET-5370             PIC  9(04).
              05 WRK-MENS-RET-5370            PIC  X(79).
              05 WRK-VJURO-OPER-DESC-5370     PIC  S9(15)V9(2).

