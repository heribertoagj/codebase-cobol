      ******************************************************************
      * SISTEMA : DCOM - I#DCOMRZ                                      *
      * TAMANHO : 0086 BYTES                                           *
      * ARQUIVO : ARQUIVO DE INTERFACE DCOM/RESP-RESPONSABILIDADE      *
      ******************************************************************
       01  RZ-REGISTRO-RESP.
           05  RZ-AGENCIA                PIC  9(05).
           05  RZ-CONTA                  PIC  9(07).
           05  RZ-NOME                   PIC  X(34).
           05  RZ-NCZ                    PIC  X(01).
           05  RZ-CART-ORIG              PIC  X(03).
           05  RZ-RESERVA                PIC  X(01).
           05  RZ-FIRMA                  PIC  X(03).
           05  RZ-CARTEIRA               PIC  X(03).
           05  RZ-VR-DCOM                PIC  9(11)V99 COMP-3.
           05  RZ-CONTRATO               PIC  9(07).
           05  RZ-DATA                   PIC  9(09)    COMP-3.
           05  RZ-NRO-PREST              PIC  9(02).
           05  RZ-VALOR-PREST            PIC  9(09)V99 COMP-3.
           05  RZ-IDENT.
               10  RZ-IDENT-SALDO-CARG   PIC  9(01).
               10  RZ-IDENT-GARANT-CARG  PIC  9(01).
