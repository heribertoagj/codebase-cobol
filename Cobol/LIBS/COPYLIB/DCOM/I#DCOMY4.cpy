      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMY4 *
      * CRIACAO   : 09/2006                                   *
      * DESCRICAO : OBTER QTDE E VALORES DOS CHEQUES          *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM7990                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-7990.
           03 WRK-ENVIO-7990.
              05 WRK-QTDE-DESP-7990             PIC  9(03).
              05 WRK-DANO-OPER-DESC-7990        PIC  9(04).
              05 WRK-NSEQ-OPER-DESC-7990        PIC  9(09).
              05 WRK-CTPO-DESC-COML-7990        PIC  9(03).

      *********************************************************
      * ÔREA DE RETORNO                                       *
      *********************************************************

           03 WRK-RETORNO-7990.
              05 WRK-COD-RET-7990               PIC  9(04).
              05 WRK-MSG-RET-7990               PIC  X(79).
              05 WRK-QTD-RET-7990               PIC  9(03).
              05 WRK-PRX-RST-7990               PIC  9(03).
              05 WRK-TOTS-TITULO OCCURS 70 TIMES INDEXED BY IND-7990.
                 07 WRK-QTDE-CHEQUE-7990        PIC  9(07).
                 07 WRK-DATA-CHEQUE-7990        PIC  X(10).
                 07 WRK-VALO-CHEQUE-7990        PIC S9(15)V9(2).
