      ******************************************************************
      * NOME DA INC - GFCTWADJ                                         *
      * DESCRICAO   - ARQUIVO DO EXTRATO MENSAL                        *
      * TAMANHO     - 131                                              *
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
       01  GFCTWADJ-REG.
           03 WADJ-TIPO-REGISTRO        PIC X(001).
           03 WADJ-COD-AGENCIA          PIC 9(005).
           03 WADJ-CPF-CNPJ.
ST2507*       05 WADJ-PRINCIPAL         PIC 9(009).
ST2507*       05 WADJ-FILIAL            PIC 9(004).
ST2507        05 WADJ-PRINCIPAL         PIC X(009).
ST2507        05 WADJ-FILIAL            PIC X(004).
              05 WADJ-CTLE              PIC 9(002).
           03 WADJ-RAZAO.
              05 WADJ-GRUPO             PIC 9(003).
              05 WADJ-SUBGP             PIC 9(003).
           03 WADJ-NRO-CONTA            PIC 9(007).
           03 WADJ-NUMERO-DOCUMENTO     PIC 9(007).
           03 WADJ-COD-LANCAMENTO       PIC 9(005).
           03 WADJ-DATA-LANCAMENTO.
              05 WADJ-DT-LANCTO-DD      PIC 9(002).
              05 WADJ-DT-LANCTO-MM      PIC 9(002).
              05 WADJ-DT-LANCTO-AAAA    PIC 9(004).
           03 WADJ-DESC-LANCAMENTO      PIC X(031).
           03 WADJ-VLR-LANCAMENTO       PIC 9(012)V99.
           03 WADJ-HIST-CPL             PIC X(032).
