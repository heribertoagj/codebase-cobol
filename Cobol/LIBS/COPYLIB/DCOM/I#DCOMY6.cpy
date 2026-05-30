      *********************************************************
      * SISTEMA   : DCOM - DESCONTO COMERCIAL  NOME: I#DCOMY6 *
      * CRIACAO   : 09/2006                                   *
      * DESCRICAO : ACESSO AO MODULO CEPN9300                 *
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

       01  WRK-AREA-9300.
           03 WRK-ENVIO-9300.
              05 WRK-NOM-PGM-9300               PIC  X(08).
              05 WRK-COD-RET-9300               PIC  9(02).
              05 WRK-CCEP-9300                  PIC  9(05).
              05 WRK-CCEP-COMPL-9300            PIC  9(03).

      *********************************************************
      * AREA DE RETORNO                                       *
      *********************************************************

           03 WRK-RETORNO-9300.
              05 WRK-COD-ENTR-9300              PIC  X(01).
              05 WRK-ELOGDR-9300                PIC  X(60).
              05 WRK-IBAIRO-9300                PIC  X(20).
              05 WRK-CMUN-9300                  PIC  9(05).
              05 WRK-IMUN-9300                  PIC  X(30).
              05 WRK-CSGL-UF-9300               PIC  X(02).
              05 WRK-CDD-9300                   PIC  X(05).
              05 WRK-CBCO-DEPOS-9300            PIC  9(03).
              05 WRK-CAG-BCRIA-DEPOS-9300       PIC  9(05).
              05 WRK-FILLER-9300                PIC  X(50).

