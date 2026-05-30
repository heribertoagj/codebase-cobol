      ******************************************************************
      *    NOME BOOK : CRURW03V                                        *
      *    DESCRICAO : BOOK DE INTERFACE                               *
      *    DATA      : 18/03/2013                                      *
      *    AUTOR     : DAVI RODRIGUES                                  *
      *    EMPRESA   : CAPGEMINI                                       *
      *    GRUPO     : CAPGEMINI                                       *
      *    TAMANHO   : 1030 BYTES                                      *
      *                                                                *
      ******************************************************************
      * ALTERACAO PARA ATENDER O ENQUADRAMENTO - DEMANDA LEGAL:        *
      * INCLUIDO O CAMPO CMODLD-RURAL-BACEN                            *
      ******************************************************************
BR1018* INCLUIDO CAMPO CRURW03V-QTD-PROD-ESP PARA ATENDER SICOR 4.12   *
      ******************************************************************
      *
           05 CRURW03V-REGISTROS.
             10 CRURW03V-QCOOP-LOTE-RURAL    PIC  9(004).
             10 CRURW03V-CLOTE-IMOV-RURAL    PIC  X(010).
             10 CRURW03V-CTPO-BNEFC-RURAL    PIC  9(002).
             10 CRURW03V-CQUALF-BNEFC-RURAL  PIC  9(002).
             10 CRURW03V-CPSSOA              PIC  9(010).
             10 CRURW03V-CATVDD-RURAL        PIC  9(005).
             10 CRURW03V-CFNALD-RURAL        PIC  9(004).
             10 CRURW03V-CSUB-FNALD          PIC  9(003).
             10 CRURW03V-CORIGE-REC-CREDT    PIC  9(003).
             10 CRURW03V-NCONTR-FUMAG-RURAL  PIC  9(005).
             10 CRURW03V-CMODLD-RURAL-BACEN  PIC  9(008).
             10 CRURW03V-VLR-RECEITA-BRUTA   PIC  9(13)V9(02).
             10 CRURW03V-NDAP                PIC  X(025).
             10 CRURW03V-CD-GRUPO-DAP        PIC  X(02).
             10 CRURW03V-DVALDD-DAP          PIC  X(010).
BR1018       10 CRURW03V-QTD-PROD-ESP        PIC  X(016).
             10 FILLER                       PIC  X(916).
