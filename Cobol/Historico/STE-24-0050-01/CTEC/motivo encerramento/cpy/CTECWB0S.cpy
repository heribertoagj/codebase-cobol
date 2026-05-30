      ******************************************************************
      * NOME BOOK : CTECWB0S                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * TAMANHO   : 03052                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB0S-HEADER.
              10 CTECWB0S-COD-LAYOUT         PIC X(08) VALUE 'CTECWB0S'.
              10 CTECWB0S-TAM-LAYOUT         PIC 9(05) VALUE 03052.
           05 CTECWB0S-REGISTRO.
              10 CTECWB0S-BLOCO-SAIDA.
                 15 CTECWB0S-NUM-REG      PIC 9(02).
                 15 CTECWB0S-LISTA           OCCURS 0 
                                             TO 50 TIMES
                                             DEPENDING 
                                             ON CTECWB0S-NUM-REG.
                    20 CTECWB0S-CMOTVO-ENCRR-CTA        PIC 9(03).
                    20 CTECWB0S-RMOTVO-ENCRR-CTA        PIC X(70).
                    20 CTECWB0S-CSIT-MOTVO-ENCRR-CTA    PIC X(02).
                    20 CTECWB0S-RSIT-MOTVO-ENCRR-CTA    PIC X(30).
                    20 CTECWB0S-CINDCD-TPO-MANUT        PIC X(01).
                    20 CTECWB0S-RINDCD-TPO-MANUT        PIC X(45).
                    20 CTECWB0S-CUSUAR-INCL             PIC X(09).
                    20 CTECWB0S-RUSUAR-INCL             PIC X(60).
                    20 CTECWB0S-CUSUAR-MANUT            PIC X(09).
                    20 CTECWB0S-RUSUAR-MANUT            PIC X(60).
                    20 CTECWB0S-HINCL-REG               PIC X(26).
                    20 CTECWB0S-HMANUT-REG              PIC X(26).
