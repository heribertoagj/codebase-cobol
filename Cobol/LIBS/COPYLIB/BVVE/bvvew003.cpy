      *----------------------------------------------------------------*
      *                      Inicio da Geracao                         *
      *              DICM-CopyBook-Processo Publicacao                 *
      *----------------------------------------------------------------*
      * Copy Book - BVVEW003
      * Data/Hora : 24/Mai/2019 16:44:50
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Inicio do Include                        *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
       05 CCNPJ-CPF-FORMT                                      PIC X(20)
           VALUE 'SPACES'.
       05 IEMPRS-ORIGD-FUNDO                                   PIC X(40)
           VALUE 'SPACES'.
       05 IBCO-PRODT-TITLO-COBR                                PIC X(40)
           VALUE 'SPACES'.
       05 CCARCT-IDTFD-MSGEM                                   PIC X(10)
           VALUE 'SPACES'.
       05 QITEM-COMPS-MSGEM                                     PIC 9(3)
           VALUE ZEROS.
       05 MD-GRUP
           DEPENDING ON QINCON-ARQ-RECBD.
          10 RTEXTO-ALERT-REJTD                                PIC X(70)
              OCCURS 50 TIMES
              VALUE 'SPACES'.
       05 DLIM-APROV-LCTO-CTBIL                                PIC X(10)
           VALUE 'SPACES'.
      *----------------------------------------------------------------*
      *                       Final do Include                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                       Final da Geracao                         *
      *               Campos-CopyBook-Processo Publicacao              *
      *----------------------------------------------------------------*
