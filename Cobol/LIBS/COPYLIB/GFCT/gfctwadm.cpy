      ******************************************************************
      * NOME DA INC - GFCTWADM                                         *
      * DESCRICAO   - ARQUIVO DE EXTRATO MENSAL + IDENTIFICADOR        *
      * TAMANHO     - 150                                              *
      * DATA        - 11/2008                                          *
      * RESPONSAVEL - VINICIUS C. MADUREIRA - SONDA PROCWORK           *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWADM-REG.
           03 WADM-TIPO-REGISTRO        PIC X(001).
           03 WADM-COD-AGENCIA          PIC 9(005).
           03 WADM-CPF-CNPJ.
ST2507*       05 WADM-PRINCIPAL         PIC 9(009).
ST2507*       05 WADM-FILIAL            PIC 9(004).
ST2507        05 WADM-PRINCIPAL         PIC X(009).
ST2507        05 WADM-FILIAL            PIC X(004).
              05 WADM-CTLE              PIC 9(002).
           03 WADM-RAZAO.
              05 WADM-GRUPO             PIC 9(003).
              05 WADM-SUBGP             PIC 9(003).
           03 WADM-NRO-CONTA            PIC 9(007).
           03 WADM-NUMERO-DOCUMENTO     PIC 9(007).
           03 WADM-COD-LANCAMENTO       PIC 9(005).
           03 WADM-DATA-LANCAMENTO.
              05 WADM-DT-LANCTO-DD      PIC 9(002).
              05 WADM-DT-LANCTO-MM      PIC 9(002).
              05 WADM-DT-LANCTO-AAAA    PIC 9(004).
           03 WADM-VLR-LANCAMENTO       PIC 9(012)V99.
           03 WADM-SEGUNDA-LINHA        PIC X(032).
           03 WADM-IDENT                PIC 9(001).
           03 WADM-QTDE-TAR             PIC 9(005).
           03 FILLER                    PIC X(044).
