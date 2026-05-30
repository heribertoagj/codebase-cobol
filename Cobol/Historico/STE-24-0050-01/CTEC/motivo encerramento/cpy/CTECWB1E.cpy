      ******************************************************************
      * NOME BOOK : CTECWB1E - BOOK DE ENCTRADA CTEC140L               *
      * DATA      : 10/2024                                            *
      * AUTOR     : HERIBERTO GIANNASI                                 *
      * EMPRESA   : STEFANINI                                          *
      * COMPONENTE: COORDENADOR                                        *
      * TAMANHO   : 00000 BYTES                                        *
      ******************************************************************
           05 CTECWB1E-HEADER.
              10 CTECWB1E-COD-LAYOUT       PIC X(008) VALUE 'CTECWB1E'.
              10 CTECWB1E-TAM-LAYOUT       PIC 9(005) VALUE 00176.
           05 CTECWB1E-REGISTRO.
              10 CTECWB1E-BLOCO-ENTRADA.
                 15 CTECWB1E-CMOTVO-ENCRR-CTA        PIC 9(03).
                 15 CTECWB1E-RMOTVO-ENCRR-CTA        PIC X(70).
                 15 CTECWB1E-CTPO-ENCRR-CTA          PIC 9(02).
                 15 CTECWB1E-CTPO-CONTR-NEGOC        PIC 9(03).
                 15 CTECWB1E-CEMIS-CARTA-ENCRR       PIC X(01).
                 15 CTECWB1E-RMSGEM-MOTVO-ENCRR      PIC X(256).
      *  Campos novos       
                 15 CTECWB1E-CINDCD-ENCRR            PIC X(01).
                 15 CTECWB1E-QDIA-AGNDA-PERM         PIC 9(03).
              