      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMY5 *
      * CRIACAO   : 09/2006                                   *
      * DESCRICAO : OBTER BACO E AGENCIA DEPOSITARIA.         *
      *                                                       *
      *                                                       *
      * APLICACAO : MODULO DCOM6433                           *
      *                                                       *
      * TAMANHO   :                                           *
      *                                                       *
      *********************************************************

      *********************************************************
      * AREA DE ENTRADA                                       *
      *********************************************************

       01  WRK-AREA-6433.
           03 WRK-ENVIO-6433.
              05 WRK-CCEP-6433                  PIC  9(05).
              05 WRK-CCEP-COMPL-6433            PIC  9(03).

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************

           03 WRK-RETORNO-6433.
              05 WRK-COD-RET-6433               PIC  9(04).
              05 WRK-MSG-RET-6433               PIC  X(79).
              05 WRK-COD-ENTR-6433              PIC  X(01).
              05 WRK-ELOGDR-6433                PIC  X(60).
              05 WRK-IBAIRO-6433                PIC  X(20).
              05 WRK-CMUN-6433                  PIC  9(05).
              05 WRK-IMUN-6433                  PIC  X(30).
              05 WRK-CSGL-UF-6433               PIC  X(02).
              05 WRK-CDD-6433                   PIC  X(05).
              05 WRK-CBCO-DEPOS-6433            PIC  9(03).
              05 WRK-CAG-BCRIA-DEPOS-6433       PIC  9(05).

