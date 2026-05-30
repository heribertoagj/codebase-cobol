      ******************************************************************
      * NOME BOOK : CRURWFGI - INTERFACE DO COORDENADOR E FUNCIONAL    *
      * DESCRICAO :                                                    *
      * DATA      : 03 / 2021                                          *
      * AUTOR     : RODRIGO SANTANA                                    *
      * EMPRESA   : FOURSYS                                            *
      * TAMANHO   : 175 BYTES                                          *
      ******************************************************************
      * DATA       AUTOR             DESCRICAO / MANUTENCAO            *
      ******************************************************************
           05 CRURWFGIE-HEADER.
             10 CRURWFGI-COD-LAYOUT    PIC X(08)   VALUE 'CRURWFGI'.
             10 CRURWFGI-TAM-LAYOUT    PIC 9(05)   VALUE 00175.
           05 CRURWFGI-REGISTRO.
             10 CRURWFGI-BLOCO-ENTRADA.
               15 CRURWFGI-E-CMODLD-RURAL         PIC 9(005).
               15 CRURWFGI-E-CTPO-BNEFC-RURAL     PIC 9(002).
               15 CRURWFGI-E-CEXIGE-IMOV-BENEF    PIC X(001).
               15 CRURWFGI-E-CIDTFD-EXIGE-AREA    PIC X(001).
               15 CRURWFGI-E-CIDTFD-EXIGE-QTD     PIC X(001).
               15 CRURWFGI-E-CSIT-REG-RURAL       PIC X(001).
               15 CRURWFGI-E-CUSUAR-INCL          PIC X(009).
               15 CRURWFGI-E-HINCL-REG            PIC X(026).
               15 CRURWFGI-E-CUSUAR-MANUT         PIC X(009).
               15 CRURWFGI-E-HMANUT-REG           PIC X(026).
             10 CRURWFGI-BLOCO-SAIDA.
               15 CRURWFGI-S-CMODLD-RURAL         PIC 9(005).
               15 CRURWFGI-S-CTPO-BNEFC-RURAL     PIC 9(002).
               15 CRURWFGI-S-CEXIGE-IMOV-BENEF    PIC X(001).
               15 CRURWFGI-S-CIDTFD-EXIGE-AREA    PIC X(001).
               15 CRURWFGI-S-CIDTFD-EXIGE-QTD     PIC X(001).
               15 CRURWFGI-S-CSIT-REG-RURAL       PIC X(001).
               15 CRURWFGI-S-CUSUAR-INCL          PIC X(009).
               15 CRURWFGI-S-HINCL-REG            PIC X(026).
               15 CRURWFGI-S-CUSUAR-MANUT         PIC X(009).
               15 CRURWFGI-S-HMANUT-REG           PIC X(026).
