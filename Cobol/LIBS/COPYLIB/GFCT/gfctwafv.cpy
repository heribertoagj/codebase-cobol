      ******************************************************************
      * NOME DA INC - GFCTWAFV                                         *
      * DESCRICAO   - COMBO DE TARIFA P/ EXCLUSAO MASSIVA ADESAO PACOTE*
      *               COMUNICACAO IMS X WEB - ENTRADA                  *
      * TAMANHO     - 1010                                             *
      * DATA        - 16/02/2009                                       *
      * RESPONSAVEL - WAGNER SILVA    - PROCWORK - TEL 4506-2345       *
      ******************************************************************
      *
       01  WAFV-ENTRADA.
           03  WAFV-LL                              PIC S9(004) COMP.
           03  WAFV-ZZ                              PIC S9(004) COMP.
           03  WAFV-TRANSACAO                       PIC  X(009).
           03  WAFV-FUNCAO                          PIC  X(008).
           03  WAFV-DADOS-RST.
               05  WAFV-FILTRO.
                   07 WAFV-COD-AGRUPAMENTO          PIC  9(003).
               05  WAFV-QTDE-OCOR                   PIC  9(003).
               05  WAFV-QTDE-TOT-REG                PIC  9(006).
               05  WAFV-FUNC-BDSCO                  PIC  X(007).
               05  WAFV-TERMINAL                    PIC  X(008).
               05  WAFV-PONTEIRO.
                   07 WAFV-PONTEIRO-TARIFA          PIC  9(005).
               05  WAFV-FIM                         PIC  X(001).
               05  WAFV-TIPO-FUNCAO                 PIC  9(001).
      *            1 - DEFERIMENTO
      *            2 - CONSULTA
      *            3 - EXCLUSAO
               05  FILLER                           PIC  X(045).
