      ******************************************************************
      * NOME DA INC - GFCTWAET                                         *
      * DESCRICAO   - PREVIA DE EMISSAO DO EXTRATO ANUAL DE TARIFAS    *
      * TAMANHO     - 080                                              *
      * DATA        - 12/2008                                          *
      * RESPONSAVEL - ROGERIO F. PREVEDEL - SONDA PROCWORK             *
      *================================================================*
ST2507*               U L T I M A   A L T E R A C A O                  *
      *----------------------------------------------------------------*
ST2507*    ANALISTA....:  STEFANINI                                    *
ST2507*    DATA........:  07 / 2025                                    *
ST2507*    OBJETIVO....:  CNPJ ALFANUMERICO                            *
      *================================================================*
      *
       01  GFCTWAET-REG.
           10 WAET-ANO-PREV            PIC S9(004).
           10 WAET-AGENCIA-PREV        PIC S9(005).
           10 WAET-CONTA-PREV          PIC S9(007).
           10 WAET-QTDE-PREV           PIC S9(005).
           10 WAET-PAB                 PIC S9(005).
           10 WAET-SEGTO               PIC S9(005).
           10 WAET-RENTB               PIC S9(009)V99.
           10 WAET-RENDA               PIC S9(013)V99.
           10 WAET-INDICA-GECT         PIC  X(001).
           10 WAET-TIPO-EMIS           PIC  X(001).
           10 WAET-SEGTO-CLIE          PIC S9(003).
ST2507*    10 WAET-CCGC-CPF            PIC S9(009).
ST2507*    10 WAET-CFLIAL-CGC          PIC S9(004).
ST2507     10 WAET-CCGC-CPF            PIC  X(009).
ST2507     10 WAET-CFLIAL-CGC          PIC  X(004).
           10 WAET-CCTRL-CPF-CGC       PIC S9(002).
           10 FILLER                   PIC  X(003).
