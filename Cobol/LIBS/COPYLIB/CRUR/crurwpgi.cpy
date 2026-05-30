      ******************************************************************
      * NOME BOOK : CRURWPGI                                           *
      * DESCRICAO : BOOK INTERFACE - CRUR240G                          *
      * DATA      : 12/2024                                            *
      * AUTOR     : FABRICA                                            *
      * EMPRESA   : STEFANINI                                          *
      * GRUPO     : STEFANINI                                          *
      * COMPONENTE:                                                    *
      * TAMANHO   : 8016 BYTES                                         *
      ******************************************************************
       05 CRURWPGI-HEADER.
           10 CRURWPGI-COD-LAYOUT       PIC X(008)  VALUE 'CRURWPGI'.
           10 CRURWPGI-TAM-LAYOUT       PIC 9(005)  VALUE 08016.
       05 CRURWPGI-REGISTRO.
           10 CRURWPGI-BLOCO-ENTRADA.
             15 CRURWPGI-E-COUTRO-TPO-CONFM            PIC 9(05).
             15 CRURWPGI-E-ROUTRO-CONFM-BACEN          PIC X(80).
             15 CRURWPGI-E-CSIT-REG-RURAL              PIC X(01).
             15 CRURWPGI-E-MAX-OCORR                   PIC 9(02).
           10 CRURWPGI-BLOCO-PAGINACAO.
             15 CRURWPGI-INDICADOR-PAGINACAO           PIC X(01).
                 88 CRURWPGI-P-INICIAL                 VALUE 'I'.
                 88 CRURWPGI-P-PRIMEIRA                VALUE 'P'.
                 88 CRURWPGI-P-SEGUINTE                VALUE 'S'.
                 88 CRURWPGI-P-ANTERIOR                VALUE 'A'.
                 88 CRURWPGI-P-ULTIMA                  VALUE 'U'.
             15 CRURWPGI-CHAVE-INI.
                  20 CRURWPGI-I-COUTRO-TPO-CONFM       PIC 9(005).
             15 CRURWPGI-CHAVE-FIM.
                  20 CRURWPGI-F-COUTRO-TPO-CONFM       PIC 9(005).
             15 CRURWPGI-P-COD-RETORNO                 PIC 9(002).
           10 CRURWJ2Y-Z-BLOCO-SAIDA.
             15 CRURWPGI-NUM-OCORR                     PIC 9(002).
             15 CRURWPGI-OCORRENCIA       OCCURS 50 TIMES DEPENDING ON
                                            CRURWPGI-NUM-OCORR.
                  20 CRURWPGI-S-COUTRO-TPO-CONFM       PIC 9(05).
                  20 CRURWPGI-S-COUTRO-CONFM-BACEN     PIC 9(02).
                  20 CRURWPGI-S-ROUTRO-CONFM-BACEN     PIC X(80).
                  20 CRURWPGI-S-CSIT-REG-RURAL         PIC X(01).
                  20 CRURWPGI-S-CUSUAR-INCL            PIC X(09).
                  20 CRURWPGI-S-HINCL-REG              PIC X(26).
                  20 CRURWPGI-S-CUSUAR-MANUT           PIC X(09).
                  20 CRURWPGI-S-HMANUT-REG             PIC X(26).
