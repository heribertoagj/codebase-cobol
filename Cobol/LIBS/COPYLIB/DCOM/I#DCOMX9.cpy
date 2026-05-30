      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMX9 *
      * CRIACAO   : 08/2006                                   *
      * DESCRICAO : OBTER SEGMENTO DO CLIENTE                 *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM6432                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * ÔREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-6432.
           03 WRK-ENVIO-6432.
              05 WRK-CBCO-6432                  PIC  9(03).
              05 WRK-CAG-BCRIA-6432             PIC  9(05).
              05 WRK-CCTA-CLI-6432              PIC  9(07).

      *********************************************************
      * ÔREA DE RETORNO                                       *
      *********************************************************

           03 WRK-RETORNO-6432.
              05 WRK-COD-RET-6432               PIC  9(04).
              05 WRK-MENS-RET-6432              PIC  X(79).
              05 WRK-CSGMTO-CLI-6432            PIC  9(03).
              05 WRK-CTPO-PSSOA-6432            PIC  X(02).
