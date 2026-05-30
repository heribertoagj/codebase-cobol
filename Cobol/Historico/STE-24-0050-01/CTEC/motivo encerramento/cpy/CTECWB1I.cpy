      ******************************************************************
      * NOME BOOK : CTECWB1I                                           *
      * DESCRICAO : BOOK DE INTERFACE                                  *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: FUNCIONAL                                          *
      * TAMANHO   : 03124                                              *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CTECWB1I-HEADER.
              10 CTECWB1I-COD-LAYOUT       PIC X(008) VALUE 'CTECWB1I'.
              10 CTECWB1I-TAM-LAYOUT       PIC 9(005) VALUE 00176.
           05 CTECWB1I-REGISTRO.
              10 CTECWB1I-BLOCO-ENTRADA.
                 15 CTECWB1I-CMOTVO-ENCRR-CTA        PIC 9(03).
                 15 CTECWB1I-RMOTVO-ENCRR-CTA        PIC X(70).
                 15 CTECWB1I-CTPO-ENCRR-CTA          PIC 9(02).
                 15 CTECWB1I-CTPO-CONTR-NEGOC        PIC 9(03).
                 15 CTECWB1I-CEMIS-CARTA-ENCRR       PIC X(01).
                 15 CTECWB1I-RMSGEM-MOTVO-ENCRR      PIC X(256).
                 15 CTECWB1I-CINDCD-ENCRR            PIC X(01).
                 15 CTECWB1I-QDIA-AGNDA-PERM         PIC 9(03).
                 15 CTECWB1I-CUSUAR-INCL             PIC X(09).
              