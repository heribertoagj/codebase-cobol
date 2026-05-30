      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMY3 *
      * CRIACAO   : 09/2006                                   *
      * DESCRICAO : OBTER SEGMENTO DO CLIENTE NO CLIE         *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM7991                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-7991.
           03 WRK-ENVIO-7991.
              05 WRK-CBCO-7991                  PIC  9(03).
              05 WRK-CAG-BCRIA-7991             PIC  9(05).
              05 WRK-CCTA-CLI-7991              PIC  9(07).

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************

           03 WRK-RETORNO-7991.
              05 WRK-COD-RET-7991               PIC  9(04).
              05 WRK-MENS-RET-7991              PIC  X(79).
              05 WRK-CSGMTO-CLI-7991            PIC  9(03).
              05 WRK-CTPO-PSSOA-7991            PIC  X(02).

