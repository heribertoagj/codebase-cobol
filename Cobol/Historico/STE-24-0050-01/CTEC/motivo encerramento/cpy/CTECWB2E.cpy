      ******************************************************************
      * NOME BOOK : CTECWB2E - BOOK DE ENCTRADA CTEC140L               *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * TAMANHO   : 00000 BYTES                                        *
      ******************************************************************
           05 CTECWB2E-HEADER.
              10 CTECWB2E-COD-LAYOUT        PIC X(008) VALUE 'CTECWB2E'.
              10 CTECWB2E-TAM-LAYOUT        PIC 9(005) VALUE 00176.
           05 CTECWB2E-REGISTRO.
              10 CTECWB2E-BLOCO-ENTRADA.
                 15 CTECWB2E-CMOTVO-ENCRR-CTA        PIC 9(03).
                 15 CTECWB2E-RMOTVO-ENCRR-CTA        PIC X(70).
                 15 CTECWB2E-CTPO-ENCRR-CTA          PIC 9(02).
                 15 CTECWB2E-CTPO-CONTR-NEGOC        PIC 9(03).
                 15 CTECWB2E-CEMIS-CARTA-ENCRR       PIC X(01).
                 15 CTECWB2E-RMSGEM-MOTVO-ENCRR      PIC X(256).
      *  Campos novos       
                 15 CINDCD-ENCRR                     PIC X(01).
                 15 QDIA-AGNDA-PERM                  PIC 9(03).














