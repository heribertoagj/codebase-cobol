      ******************************************************************
      * NOME BOOK : CTECWB3S                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * TAMANHO   : 03052                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB3S-HEADER.
              10 CTECWB3S-COD-LAYOUT         PIC X(08) VALUE 'CTECWB3S'.
              10 CTECWB3S-TAM-LAYOUT         PIC 9(05) VALUE 03052.
           05 CTECWJCS-REGISTRO.
              10 CTECWB3S-BLOCO-SAIDA.
                 15 CTECWB3S-CMOTVO-ENCRR-CTA        PIC 9(03).
                 15 CTECWB3S-RMOTVO-ENCRR-CTA        PIC X(70).
                 15 CTECWB3S-CSIT-MOTVO-ENCRR-CTA    PIC X(02).
                 15 CTECWB3S-RSIT-MOTVO-ENCRR-CTA    PIC X(30).
                 15 CTECWB3S-CINDCD-TPO-MANUT        PIC X(01).
                 15 CTECWB3S-RINDCD-TPO-MANUT        PIC X(45).
                 15 CTECWB3S-CTPO-ENCRR-CTA          PIC 9(02).
                 15 CTECWB3S-CTPO-CONTR-NEGOC        PIC 9(03).
                 15 CTECWB3S-CEMIS-CARTA-ENCRR       PIC X(01).
                 15 CTECWB3S-CINDCD-ENCRR            PIC X(01).
                 15 CTECWB3S-QDIA-AGNDA-PERM         PIC 9(03).
                 15 CTECWB3S-RMSGEM-MOTVO-ENCRR      PIC X(255).
                 15 CTECWB3S-CUSUAR-INCL             PIC X(09).
                 15 CTECWB3S-RUSUAR-INCL             PIC X(60).
                 15 CTECWB3S-CUSUAR-MANUT            PIC X(09).
                 15 CTECWB3S-RUSUAR-MANUT            PIC X(60).
                 15 CTECWB3S-HINCL-REG               PIC X(26).
                 15 CTECWB3S-HMANUT-REG              PIC X(26).