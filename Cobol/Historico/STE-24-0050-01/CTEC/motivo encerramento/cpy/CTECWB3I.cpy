      ******************************************************************
      * NOME BOOK : CTECWB3I                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 02/2020                                            *
      * AUTOR     : MAURICIO VIEIRA JR                                 *
      * EMPRESA   : TCS CONSULTANCY SERVICES                           *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 03124                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB3I-HEADER.
              10 CTECWB3I-COD-LAYOUT         PIC X(08) VALUE 'CTECWB3I'.
              10 CTECWB3I-TAM-LAYOUT         PIC 9(05) VALUE 03124.
           05 CTECWJCI-REGISTRO.
              10 CTECWB3I-BLOCO-ENTRADA.
                 15 CTECWB3I-CMOVTO-ENCERR           PIC 9(03).
                 15 CTECWB3I-HINCL-REG               PIC X(26).
              10    CTECWB3I-BLOCO-SAIDA.
                 15 CTECWB3I-CMOTVO-ENCRR-CTA        PIC 9(03).
                 15 CTECWB3I-RMOTVO-ENCRR-CTA        PIC X(70).
                 15 CTECWB3I-CSIT-MOTVO-ENCRR-CTA    PIC X(02).
                 15 CTECWB3I-RSIT-MOTVO-ENCRR-CTA    PIC X(30).
                 15 CTECWB3I-CINDCD-TPO-MANUT        PIC X(01).
                 15 CTECWB3I-RINDCD-TPO-MANUT        PIC X(45).
                 15 CTECWB3I-CTPO-ENCRR-CTA          PIC 9(02).
                 15 CTECWB3I-CTPO-CONTR-NEGOC        PIC 9(03).
                 15 CTECWB3I-CEMIS-CARTA-ENCRR       PIC X(01).
                 15 CTECWB3I-CINDCD-ENCRR            PIC X(01).
                 15 CTECWB3I-QDIA-AGNDA-PERM         PIC 9(03).
                 15 CTECWB3I-RMSGEM-MOTVO-ENCRR      PIC X(255).
                 15 CTECWB3I-CUSUAR-INCL             PIC X(09).
                 15 CTECWB3I-RUSUAR-INCL             PIC X(60).
                 15 CTECWB3I-CUSUAR-MANUT            PIC X(09).
                 15 CTECWB3I-RUSUAR-MANUT            PIC X(60).
                 15 CTECWB3I-HINCL-REG               PIC X(26).
                 15 CTECWB3I-HMANUT-REG              PIC X(26).
