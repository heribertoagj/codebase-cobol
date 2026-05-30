      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMX8 *
      * CRIACAO   : 08/2006                                   *
      * DESCRICAO : CALCULAR PRAZO MEDIO PONDERADO            *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM5358                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * ÙREA DE ENTRADA                                       *
      *********************************************************
       01  WRK-AREA-5358.
           03 WRK-ENVIO-5358.
              05 WRK-DANO-OPER-DESC-5358        PIC  9(04).
              05 WRK-NSEQ-OPER-DESC-5358        PIC  9(09).
              05 WRK-CTPO-DESC-COML-5358        PIC  9(03).
              05 WRK-DINIC-OPER-DESC-5358       PIC  X(10).
      *********************************************************
      * ÙREA DE RETORNO                                       *
      *********************************************************
           03 WRK-RETORNO-5358.
              05 WRK-COD-RET-5358               PIC  9(04).
              05 WRK-MENS-RET-5358              PIC  X(79).
              05 WRK-TMED-PONDE-OPER-5358       PIC  9(03).
