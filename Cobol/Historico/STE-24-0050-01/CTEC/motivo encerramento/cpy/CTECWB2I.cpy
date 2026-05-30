      ******************************************************************
      * NOME BOOK : CTECWB2I                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 02/2020                                            *
      * AUTOR     : MAURICIO VIEIRA JR                                 *
      * EMPRESA   : TCS CONSULTANCY SERVICES                           *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 03124                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB2I-HEADER.
              10 CTECWB2I-COD-LAYOUT        PIC X(008) VALUE 'CTECWB2I'.
              10 CTECWB2I-TAM-LAYOUT        PIC 9(005) VALUE 00176.
           05 CTECWB2I-REGISTRO.
              10 CTECWB2I-BLOCO-ENTRADA.
                 15 CTECWB2I-CMOTVO-ENCRR-CTA        PIC 9(03).
                 15 CTECWB2I-RMOTVO-ENCRR-CTA        PIC X(70).
                 15 CTECWB2I-CTPO-ENCRR-CTA          PIC 9(02).
                 15 CTECWB2I-CTPO-CONTR-NEGOC        PIC 9(03).
                 15 CTECWB2I-CEMIS-CARTA-ENCRR       PIC X(01).
                 15 CTECWB2I-RMSGEM-MOTVO-ENCRR      PIC X(256).
                 15 CTECWB2I-CINDCD-ENCRR            PIC X(01).
                 15 CTECWB2I-QDIA-AGNDA-PERM         PIC 9(03).
                 15 CTECWB2I-CUSUAR-MANUT            PIC X(09).
