      ******************************************************************
      * NOME DA INC - GFCTWADK                                         *
      * DESCRICAO   - ARQUIVO DE EXTRATO MENSAL + DESCRI. ERRO         *
      * TAMANHO     - 171                                              *
      * DATA        - 11/2008                                          *
      * RESPONSAVEL - VINICIUS C. MADUREIRA - SONDA PROCWORK           *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWADK-REG.
           03 WADK-TIPO-REGISTRO        PIC X(001).
           03 WADK-COD-AGENCIA          PIC 9(005).
           03 WADK-CPF-CNPJ.
ST2507*       05 WADK-PRINCIPAL         PIC 9(009).
ST2507*       05 WADK-FILIAL            PIC 9(004).
ST2507        05 WADK-PRINCIPAL         PIC X(009).
ST2507        05 WADK-FILIAL            PIC X(004).
              05 WADK-CTLE              PIC 9(002).
           03 WADK-RAZAO.
              05 WADK-GRUPO             PIC 9(003).
              05 WADK-SUBGP             PIC 9(003).
           03 WADK-NRO-CONTA            PIC 9(007).
           03 WADK-NUMERO-DOCUMENTO     PIC 9(007).
           03 WADK-COD-LANCAMENTO       PIC 9(005).
           03 WADK-DATA-LANCAMENTO.
              05 WADK-DT-LANCTO-DD      PIC 9(002).
              05 WADK-DT-LANCTO-MM      PIC 9(002).
              05 WADK-DT-LANCTO-AAAA    PIC 9(004).
           03 WADK-DESC-LANCAMENTO      PIC X(031).
           03 WADK-VLR-LANCAMENTO       PIC 9(012)V99.
           03 WADK-HIST-CPL             PIC X(032).
           03 WADK-DESC-ERRO            PIC X(040).
