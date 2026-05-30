      ******************************************************************
      * NOME BOOK : CRURW236                                           *
      * DESCRICAO : BOOK INTERFACE - CRUR236G                          *
      * DATA      : 03/2025                                            *
      * AUTOR     : FABRICA                                            *
      * EMPRESA   : STEFANINI                                          *
      * GRUPO     : STEFANINI                                          *
      * COMPONENTE:                                                    *
      * TAMANHO   : 0056 BYTES                                         *
      ******************************************************************
       05 CRURW236-HEADER.
           10 CRURW236-COD-LAYOUT       PIC X(008)     VALUE 'CRURW236'.
           10 CRURW236-TAM-LAYOUT       PIC 9(005)     VALUE 00056.
       05 CRURW236-REGISTRO.
           10 CRURW236-BLOCO-ENTRADA.
             15 CRURW236-E-CPRODT-RURAL                PIC 9(003).
             15 CRURW236-E-CUND-ANMAL-RURAL            PIC 9(008).
             15 CRURW236-E-CMODLD                      PIC 9(002).
             15 CRURW236-E-CFNALD-RURAL                PIC 9(004).
             15 CRURW236-E-CEMPTO-RURAL                PIC 9(006).
             15 CRURW236-E-CATVDD-RURAL                PIC 9(005).
             15 CRURW236-E-CVARDE-RURAL-BACEN          PIC 9(005).
             15 CRURW236-E-CCESTA-SAFRA-BACEN          PIC 9(005).
             15 CRURW236-E-CZONA-RURAL                 PIC 9(001).
             15 CRURW236-E-CCSCIO-EMPTO-RURAL          PIC 9(001).
           10 CRURW236-BLOCO-SAIDA.
             15 CRURW236-S-COD-RETORNO                 PIC 9(002).
             15 CRURW236-S-COD-DEPARA                  PIC X(001).
      * S = CODIGO DEPARA IGUAL
      * N = CODIGO DEPARA NAO IGUAL
