      ******************************************************************
      * NOME BOOK : GFCTWK7C                                           *
      * DESCRICAO : LAYOUT DE ENTRADA                                  *
      * DATA      : 09/08/2019                                         *
      * EMPRESA   : CAPGEMINI                                          *
      * GRUPO     : GFCT                                               *
      * TAMANHO   : 057                                                *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTK7-ENTRADA.
           03 GFCTK7-FIXO.
              05 GFCTK7-CPACOTE                    PIC  9(005).
              05 GFCTK7-CPACOTE-R                   REDEFINES
                   GFCTK7-CPACOTE                     PIC  X(005).


              05 GFCTK7-CAGPTO                     PIC  9(003).
              05 GFCTK7-CAGPTO-R                    REDEFINES
                   GFCTK7-CAGPTO                      PIC  X(003).

ST2507*       05 GFCTK7-CGC-CPF                    PIC  9(009).
ST2507        05 GFCTK7-CGC-CPF                    PIC  X(009).
ST2507*       05 GFCTK7-CGC-CPF-R                   REDEFINES
ST2507*            GFCTK7-CGC-CPF                  PIC  X(009).

ST2507*       05 GFCTK7-FILIAL                     PIC  9(005).
ST2507        05 GFCTK7-FILIAL                     PIC  X(005).
ST2507*       05 GFCTK7-FILIAL-R                    REDEFINES
ST2507*            GFCTK7-FILIAL                   PIC  X(005).

              05 GFCTK7-CONTROLE                   PIC  X(002).

              05 GFCTK7-DATA-INICIO                PIC  X(010).
              05 GFCTK7-DATA-FIM                   PIC  X(010).
              05 GFCTK7-TIPO-ADESAO                PIC  X(001).
      *
      **********************************************************
      * TIPO-ADESAO: I - INDIVIDUAL  C - COMPULSORIA  A - AMBAS
      **********************************************************
              05 GFCTK7-SOLICITA-EXCECAO           PIC  9(001).
              05 GFCTK7-SOLICITA-EXCECAO-R          REDEFINES
                   GFCTK7-SOLICITA-EXCECAO            PIC  X(001).


      **********************************************************
      * SE TIPO-ADESAO = C - COMPULSORIA  OU  A - AMBAS
      *   PODERA SER:  0 - SEM EXCECAO  OU  1 - COM EXCECAO
      **********************************************************
              05 GFCTK7-PRMSS-INDVD                PIC  9(001).
              05 GFCTK7-PRMSS-INDVD-R               REDEFINES
                   GFCTK7-PRMSS-INDVD                 PIC  X(001).

      **********************************************************
      * GERA CREDITO: 0 - NAO GERA   OU  1 - GERA CREDITO
      **********************************************************
              05 GFCTK7-CPSSOA-TARIFA              PIC  X(001).
      *          '1' - FISICA
      *          '2' - JURIDICA
      *          '3' - AMBAS
              05 GFCTK7-VISUAL-COMPULS             PIC  9(001).
              05 GFCTK7-VISUAL-COMPULS-R            REDEFINES
                   GFCTK7-VISUAL-COMPULS              PIC  X(001).

      *           0  - NAO VISUALIZA COMPULSORIA
      *           1  - VISUALIZA COMPULSORIA
              05 FILLER                            PIC  X(008).
